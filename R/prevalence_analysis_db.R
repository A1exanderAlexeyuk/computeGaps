#' Analyze Prevalence from TSV with Database Storage
#'
#' This function reads TSV data containing cohort and diagnostic test information,
#' performs prevalence analysis, and stores results in the database.
#'
#' @param tsv_file_path Character. Path to the TSV file containing the analysis data
#' @param connection Database connection object (DatabaseConnector or DBI)
#' @param cohort_database_schema Character. Schema name where cohort table and results will be stored
#' @param cohort_table_name Character. Name of the cohort table (default: "cohort")
#' @param results_table_name Character. Name for the results table (default: "prevalence_analysis_results")
#' @param cdm_database_schema Character. Schema name for CDM tables (default: same as cohort_database_schema)
#' @param overwrite_results Logical. Whether to overwrite existing results table (default: TRUE)
#' @param create_indexes Logical. Whether to create indexes on results table (default: TRUE)
#' @param verbose Logical. Whether to print progress messages (default: TRUE)
#'
#' @return Character. Name of the created results table
#'
#' @export
analyze_prevalence_from_tsv_db <- function(tsv_file_path,
                                           connection,
                                           cohort_database_schema,
                                           cohort_table_name = "cohort",
                                           results_table_name = "prevalence_analysis_results",
                                           cdm_database_schema = NULL,
                                           overwrite_results = TRUE,
                                           create_indexes = TRUE,
                                           verbose = TRUE) {

  # Validate inputs
  if (!file.exists(tsv_file_path)) {
    stop("TSV file not found: ", tsv_file_path)
  }

  if (is.null(cdm_database_schema)) {
    cdm_database_schema <- cohort_database_schema
  }

  if (verbose) {
    message("Starting prevalence analysis from TSV...")
    message("Reading TSV file: ", tsv_file_path)
  }

  ref_table <- DatabaseConnector::querySql(
    connection,
    sql = "select cohort_definition_id, cohort_name
    from insight_gateway_app.reference_cohorts") |>
    dplyr::rename_all(tolower)

  # Read and validate TSV data
  tsv_data <- read_and_validate_tsv(tsv_file_path) |>
    dplyr::inner_join(
      ref_table, dplyr::join_by(cohortname == cohort_name)) |>
    dplyr::mutate(
      concept_id_1 = cohort_definition_id
    ) |> dplyr::select(-cohort_definition_id)

  if (verbose) {
    message("Processing ", nrow(tsv_data), " rows of analysis data")
  }

  # Parse metadata and prepare analysis parameters
  analysis_params <- parse_analysis_metadata(tsv_data)

  # Check if results table exists and handle accordingly
  full_results_table_name <- paste0(cohort_database_schema, ".", results_table_name)


  # Group analysis by cohortname and object_custom_name
  grouped_analyses <- analysis_params |>
    dplyr::group_by(cohortname, object_custom_name, omop_object_domain, workflow_stage) |>
    dplyr::summarise(
      concept_id_1 = dplyr::first(concept_id_1),
      concept_id_2_list = list(unique(concept_id_2)),
      days_around_index_1 = dplyr::first(days_around_index_1),
      days_around_index_2 = dplyr::first(days_around_index_2),
      .groups = "drop"
    )

  if (verbose) {
    message("Processing ", nrow(grouped_analyses), " unique analysis groups...")
  }

  # Process each group
  for (i in seq_len(nrow(grouped_analyses))) {
    analysis_group <- grouped_analyses[i, ]

    if (verbose) {
      message("Processing analysis ", i, "/", nrow(grouped_analyses),
              ": ", analysis_group$cohortname, " - ", analysis_group$object_custom_name)
    }

    # Perform prevalence calculation for the group
    results <- calculate_prevalence_db_grouped(
      connection = connection,
      cohort_database_schema = cohort_database_schema,
      cdm_database_schema = cdm_database_schema,
      cohort_table_name = cohort_table_name,
      analysis_group = analysis_group
    )

    # Insert results into database
    if (nrow(results) > 0) {
      insert_results_to_db(
        connection = connection,
        cohort_database_schema = cohort_database_schema,
        results_table_name = results_table_name,
        results_data = results
      )
    }
  }

  # Create indexes for better performance
  if (create_indexes) {
    if (verbose) message("Creating indexes on results table...")
    create_results_indexes(connection, cohort_database_schema, results_table_name)
  }

  # Get final row count
  final_count <- get_table_row_count(connection, cohort_database_schema, results_table_name)

  if (verbose) {
    message("Analysis completed successfully!")
    message("Results stored in: ", full_results_table_name)
    message("Total result rows: ", final_count)
  }

  return(full_results_table_name)
}

#' Calculate Prevalence Using Database Operations for Grouped Data
#'
#' @param connection Database connection
#' @param cohort_database_schema Schema for cohort table
#' @param cdm_database_schema Schema for CDM tables
#' @param cohort_table_name Name of cohort table
#' @param analysis_group Data frame with grouped analysis parameters
#'
#' @return Data frame with prevalence results
#' @keywords internal
calculate_prevalence_db_grouped <- function(connection,
                                            cohort_database_schema,
                                            cdm_database_schema,
                                            cohort_table_name,
                                            analysis_group) {

  # Build SQL query for grouped concepts
  sql_query <- build_prevalence_sql_grouped(
    cohort_database_schema = cohort_database_schema,
    cdm_database_schema = cdm_database_schema,
    cohort_table_name = cohort_table_name,
    concept_id_1 = analysis_group$concept_id_1,
    concept_id_2_list = unlist(analysis_group$concept_id_2_list),
    domain = analysis_group$omop_object_domain,
    days_start = analysis_group$days_around_index_1,
    days_end = analysis_group$days_around_index_2
  )

  # Execute query
  query_result <- DatabaseConnector::querySql(connection, sql_query)

  if (nrow(query_result) > 0) {
    # Prepare result row
    result_row <- dplyr::tibble(
      cohortname = analysis_group$cohortname,
      omop_object_domain = analysis_group$omop_object_domain,
      object_custom_name = analysis_group$object_custom_name,
      workflow_stage = analysis_group$workflow_stage,
      concept_id_1 = analysis_group$concept_id_1,
      concept_id_2 = paste(unlist(analysis_group$concept_id_2_list), collapse = ","),
      n_patients = query_result$N_PATIENTS[1],
      n_patients_with_op = query_result$N_PATIENTS_WITH_OP[1],
      patient_count = ifelse(query_result$N_PATIENTS[1] > 0,
                             round((query_result$N_PATIENTS_WITH_OP[1] / query_result$N_PATIENTS[1]) * 100, 2),
                             0),
      days_around_index_1 = analysis_group$days_around_index_1,
      days_around_index_2 = analysis_group$days_around_index_2,
      analysis_date = Sys.Date(),
      stringsAsFactors = FALSE
    )

    return(result_row)
  } else {
    return(data.frame())
  }
}

#' Build SQL Query for Grouped Prevalence Analysis
#'
#' @param cohort_database_schema Schema for cohort table
#' @param cdm_database_schema Schema for CDM tables
#' @param cohort_table_name Name of cohort table
#' @param concept_id_1 Cohort concept ID
#' @param concept_id_2_list List of test/procedure concept IDs
#' @param domain OMOP domain (Procedure, Measurement, etc.)
#' @param days_start Start of time window
#' @param days_end End of time window
#'
#' @return Character SQL query
#' @keywords internal
build_prevalence_sql_grouped <- function(cohort_database_schema,
                                         cdm_database_schema,
                                         cohort_table_name,
                                         concept_id_1,
                                         concept_id_2_list,
                                         domain,
                                         days_start,
                                         days_end) {

  # Map domain to table name
  domain_table_map <- list(
    "Procedure" = "procedure_occurrence",
    "Measurement" = "measurement",
    "Condition" = "condition_occurrence",
    "Drug" = "drug_exposure",
    "Device" = "device_exposure",
    "Observation" = "observation"
  )

  table_name <- domain_table_map[[domain]]
  if (is.null(table_name)) {
    stop("Unsupported domain: ", domain)
  }

  # Determine concept column name based on domain
  concept_column <- switch(domain,
                           "Procedure" = "procedure_concept_id",
                           "Measurement" = "measurement_concept_id",
                           "Condition" = "condition_concept_id",
                           "Drug" = "drug_concept_id",
                           "Device" = "device_concept_id",
                           "Observation" = "observation_concept_id"
  )

  # Determine date column name based on domain
  date_column <- switch(domain,
                        "Procedure" = "procedure_date",
                        "Measurement" = "measurement_date",
                        "Condition" = "condition_start_date",
                        "Drug" = "drug_exposure_start_date",
                        "Device" = "device_exposure_start_date",
                        "Observation" = "observation_date"
  )

  # Convert concept list to string for SQL
  concept_id_2_string <- paste0("(", paste(concept_id_2_list, collapse = ","), ")")

  sql_template <- "
  WITH cohort_patients AS (
    SELECT
      subject_id,
      cohort_start_date,
      cohort_definition_id
    FROM @cohort_database_schema.@cohort_table_name
    WHERE cohort_definition_id = @concept_id_1
  ),
  patients_with_procedure AS (
    SELECT DISTINCT
      cp.subject_id,
      cp.cohort_start_date
    FROM cohort_patients cp
    INNER JOIN @cdm_database_schema.@table_name p
      ON cp.subject_id = p.person_id
    WHERE p.@concept_column IN @concept_id_2_string
      AND p.@date_column >= DATEADD(day, @days_start, cp.cohort_start_date)
      AND p.@date_column <= DATEADD(day, @days_end, cp.cohort_start_date)
  )
  SELECT
    COUNT(DISTINCT cp.subject_id) as N_PATIENTS,
    COUNT(DISTINCT pwp.subject_id) as N_PATIENTS_WITH_OP
  FROM cohort_patients cp
  LEFT JOIN patients_with_procedure pwp
    ON cp.subject_id = pwp.subject_id;
  "

  # Render the SQL
  sql_query <- SqlRender::render(
    sql = sql_template,
    cohort_database_schema = cohort_database_schema,
    cohort_table_name = cohort_table_name,
    cdm_database_schema = cdm_database_schema,
    table_name = table_name,
    concept_column = concept_column,
    date_column = date_column,
    concept_id_1 = concept_id_1,
    concept_id_2_string = concept_id_2_string,
    days_start = days_start,
    days_end = days_end
  )

  # Translate to specific dialect if needed
  sql_query <- SqlRender::translate(
    sql = sql_query,
    targetDialect = DatabaseConnector::dbms(connection)
  )

  return(sql_query)
}
