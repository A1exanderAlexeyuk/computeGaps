#' Database-Sufficient Prevalence Analysis from TSV
#'
#' This function reads TSV data, performs prevalence analysis, and stores results
#' directly in the database within the specified cohort_database_schema.
#'
#' @param tsv_file_path Character. Path to the TSV file containing cohort and test data
#' @param connection Database connection object (DBI compatible)
#' @param cohort_database_schema Character. Database schema where cohort tables are stored
#' @param cohort_table_name Character. Name of the cohort table (default: "cohort")
#' @param result_table_name Character. Name for the results table (default: "prevalence_analysis_results")
#' @param cdm_database_schema Character. Schema containing OMOP CDM tables (default: same as cohort_database_schema)
#' @param overwrite_results Logical. Whether to overwrite existing results table (default: TRUE)
#' @param chunk_size Integer. Number of rows to process at once for large datasets (default: 1000)
#' @param verbose Logical. Whether to print progress messages (default: TRUE)
#'
#' @return Character. Name of the created results table
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' result_table <- analyze_prevalence_db_sufficient(
#'   tsv_file_path = "data/cohort_tests.tsv",
#'   connection = con,
#'   cohort_database_schema = "results",
#'   cohort_table_name = "my_cohort"
#' )
#' 
#' # With custom CDM schema
#' result_table <- analyze_prevalence_db_sufficient(
#'   tsv_file_path = "data/cohort_tests.tsv",
#'   connection = con,
#'   cohort_database_schema = "results",
#'   cdm_database_schema = "cdm",
#'   result_table_name = "ibd_prevalence_results"
#' )
#' }
analyze_prevalence_db_sufficient <- function(tsv_file_path,
                                           connection,
                                           cohort_database_schema,
                                           cohort_table_name = "cohort",
                                           result_table_name = "prevalence_analysis_results",
                                           cdm_database_schema = NULL,
                                           overwrite_results = TRUE,
                                           chunk_size = 1000,
                                           verbose = TRUE) {
  
  # Validate inputs
  if (!file.exists(tsv_file_path)) {
    stop("TSV file not found: ", tsv_file_path)
  }
  
  if (!DBI::dbIsValid(connection)) {
    stop("Invalid database connection")
  }
  
  if (is.null(cdm_database_schema)) {
    cdm_database_schema <- cohort_database_schema
  }
  
  if (verbose) {
    cat("Starting database-sufficient prevalence analysis...\n")
    cat("TSV file:", tsv_file_path, "\n")
    cat("Cohort schema:", cohort_database_schema, "\n")
    cat("CDM schema:", cdm_database_schema, "\n")
  }
  
  # Read and validate TSV data
  if (verbose) cat("Reading TSV data...\n")
  tsv_data <- readr::read_tsv(tsv_file_path, show_col_types = FALSE)
  
  # Validate TSV structure
  validate_tsv_structure_db(tsv_data)
  
  # Parse metadata and prepare analysis data
  if (verbose) cat("Parsing metadata...\n")
  analysis_data <- parse_tsv_metadata_db(tsv_data)
  
  # Create results table structure
  if (verbose) cat("Creating results table structure...\n")
  create_results_table_db(connection, cohort_database_schema, result_table_name, overwrite_results)
  
  # Process data in chunks
  if (verbose) cat("Processing analysis in chunks...\n")
  total_rows <- nrow(analysis_data)
  chunks <- split(analysis_data, ceiling(seq_len(total_rows) / chunk_size))
  
  for (i in seq_along(chunks)) {
    if (verbose) cat("Processing chunk", i, "of", length(chunks), "\n")
    
    chunk_results <- process_prevalence_chunk_db(
      chunk_data = chunks[[i]],
      connection = connection,
      cohort_database_schema = cohort_database_schema,
      cdm_database_schema = cdm_database_schema,
      cohort_table_name = cohort_table_name
    )
    
    # Insert results into database
    insert_results_to_db(
      connection = connection,
      cohort_database_schema = cohort_database_schema,
      result_table_name = result_table_name,
      results_data = chunk_results
    )
  }
  
  # Create summary statistics
  if (verbose) cat("Creating summary statistics...\n")
  create_summary_table_db(connection, cohort_database_schema, result_table_name)
  
  if (verbose) {
    cat("Analysis completed successfully!\n")
    cat("Results stored in:", paste0(cohort_database_schema, ".", result_table_name), "\n")
  }
  
  return(paste0(cohort_database_schema, ".", result_table_name))
}

#' Validate TSV Structure for Database Analysis
#'
#' @param tsv_data Data frame containing TSV data
#' @return NULL (throws error if validation fails)
validate_tsv_structure_db <- function(tsv_data) {
  required_columns <- c(
    "cohortname", "concept_id_1", "relationship_id", "concept_id_2",
    "omop_object_domain", "object_custom_name", "object_custom_code",
    "predicate_metadata"
  )
  
  missing_columns <- setdiff(required_columns, names(tsv_data))
  if (length(missing_columns) > 0) {
    stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
  }
  
  # Validate concept IDs are numeric
  if (!is.numeric(tsv_data$concept_id_1) || !is.numeric(tsv_data$concept_id_2)) {
    stop("concept_id_1 and concept_id_2 must be numeric")
  }
  
  # Validate domains
  supported_domains <- c("Procedure", "Measurement", "Condition", "Drug", "Device", "Observation")
  invalid_domains <- setdiff(unique(tsv_data$omop_object_domain), supported_domains)
  if (length(invalid_domains) > 0) {
    stop("Unsupported OMOP domains: ", paste(invalid_domains, collapse = ", "))
  }
}

#' Parse TSV Metadata for Database Analysis
#'
#' @param tsv_data Data frame containing TSV data
#' @return Data frame with parsed metadata
parse_tsv_metadata_db <- function(tsv_data) {
  result <- tsv_data
  
  # Parse JSON metadata
  result$parsed_metadata <- lapply(tsv_data$predicate_metadata, function(x) {
    tryCatch({
      jsonlite::fromJSON(x)
    }, error = function(e) {
      warning("Failed to parse JSON metadata: ", x)
      list()
    })
  })
  
  # Extract key fields
  result$workflow_stage <- sapply(result$parsed_metadata, function(x) {
    if ("Workflow stage" %in% names(x)) x[["Workflow stage"]] else "Unknown"
  })
  
  result$time_gap_days <- sapply(result$parsed_metadata, function(x) {
    if ("time_gap_in_days" %in% names(x)) as.numeric(x[["time_gap_in_days"]]) else 0
  })
  
  result$days_around_index_1 <- sapply(result$parsed_metadata, function(x) {
    if ("days_around_index_1" %in% names(x)) as.numeric(x[["days_around_index_1"]]) else -30
  })
  
  result$days_around_index_2 <- sapply(result$parsed_metadata, function(x) {
    if ("days_around_index_2" %in% names(x)) as.numeric(x[["days_around_index_2"]]) else 30
  })
  
  return(result)
}

#' Create Results Table in Database
#'
#' @param connection Database connection
#' @param schema Database schema
#' @param table_name Table name
#' @param overwrite Whether to overwrite existing table
create_results_table_db <- function(connection, schema, table_name, overwrite = TRUE) {
  full_table_name <- paste0(schema, ".", table_name)
  
  # Drop table if it exists and overwrite is TRUE
  if (overwrite) {
    tryCatch({
      DBI::dbExecute(connection, paste("DROP TABLE IF EXISTS", full_table_name))
    }, error = function(e) {
      # Ignore error if table doesn't exist
    })
  }
  
  # Create table structure
  create_sql <- paste0("
    CREATE TABLE ", full_table_name, " (
      analysis_id BIGINT IDENTITY(1,1) PRIMARY KEY,
      cohortname VARCHAR(255) NOT NULL,
      omop_object_domain VARCHAR(50) NOT NULL,
      object_custom_name VARCHAR(255) NOT NULL,
      object_custom_code VARCHAR(50),
      workflow_stage VARCHAR(100),
      concept_id_1 BIGINT,
      concept_id_2 BIGINT,
      relationship_id VARCHAR(50),
      days_around_index_1 INT,
      days_around_index_2 INT,
      time_gap_days INT,
      patient_count DECIMAL(10,4),
      n_patients BIGINT,
      n_patients_with_op BIGINT,
      analysis_date DATETIME2 DEFAULT GETDATE(),
      INDEX IX_prevalence_cohort_domain (cohortname, omop_object_domain),
      INDEX IX_prevalence_workflow (workflow_stage),
      INDEX IX_prevalence_concepts (concept_id_1, concept_id_2)
    )
  ")
  
  DBI::dbExecute(connection, create_sql)
}

#' Process Prevalence Analysis for Data Chunk
#'
#' @param chunk_data Data frame chunk to process
#' @param connection Database connection
#' @param cohort_database_schema Cohort schema
#' @param cdm_database_schema CDM schema
#' @param cohort_table_name Cohort table name
#' @return Data frame with results
process_prevalence_chunk_db <- function(chunk_data, connection, cohort_database_schema, 
                                       cdm_database_schema, cohort_table_name) {
  
  results_list <- list()
  
  for (i in seq_len(nrow(chunk_data))) {
    row <- chunk_data[i, ]
    
    # Get domain table name
    domain_table <- get_domain_table_name(row$omop_object_domain)
    
    # Build and execute prevalence query
    prevalence_result <- execute_prevalence_query_db(
      connection = connection,
      cohort_schema = cohort_database_schema,
      cdm_schema = cdm_database_schema,
      cohort_table = cohort_table_name,
      domain_table = domain_table,
      concept_id_2 = row$concept_id_2,
      days_start = row$days_around_index_1,
      days_end = row$days_around_index_2,
      cohortname = row$cohortname
    )
    
    # Prepare result row
    result_row <- data.frame(
      cohortname = row$cohortname,
      omop_object_domain = row$omop_object_domain,
      object_custom_name = row$object_custom_name,
      object_custom_code = row$object_custom_code,
      workflow_stage = row$workflow_stage,
      concept_id_1 = row$concept_id_1,
      concept_id_2 = row$concept_id_2,
      relationship_id = row$relationship_id,
      days_around_index_1 = row$days_around_index_1,
      days_around_index_2 = row$days_around_index_2,
      time_gap_days = row$time_gap_days,
      patient_count = prevalence_result$prevalence_pct,
      n_patients = prevalence_result$total_patients,
      n_patients_with_op = prevalence_result$patients_with_test,
      stringsAsFactors = FALSE
    )
    
    results_list[[i]] <- result_row
  }
  
  return(do.call(rbind, results_list))
}

#' Execute Prevalence Query in Database
#'
#' @param connection Database connection
#' @param cohort_schema Cohort schema
#' @param cdm_schema CDM schema
#' @param cohort_table Cohort table name
#' @param domain_table Domain table name
#' @param concept_id_2 Concept ID to search for
#' @param days_start Start of time window
#' @param days_end End of time window
#' @param cohortname Cohort name filter
#' @return List with prevalence statistics
execute_prevalence_query_db <- function(connection, cohort_schema, cdm_schema, cohort_table,
                                       domain_table, concept_id_2, days_start, days_end, cohortname) {
  
  # Get date column name based on domain
  date_column <- get_domain_date_column(domain_table)
  concept_column <- get_domain_concept_column(domain_table)
  
  # Build prevalence query
  prevalence_sql <- paste0("
    WITH cohort_patients AS (
      SELECT subject_id, cohort_start_date
      FROM ", cohort_schema, ".", cohort_table, "
      WHERE cohort_definition_id = (
        SELECT TOP 1 cohort_definition_id 
        FROM ", cohort_schema, ".", cohort_table, "
        -- Add cohortname filter if available in cohort table
      )
    ),
    patients_with_test AS (
      SELECT DISTINCT cp.subject_id
      FROM cohort_patients cp
      INNER JOIN ", cdm_schema, ".", domain_table, " dt
        ON cp.subject_id = dt.person_id
      WHERE dt.", concept_column, " = ", concept_id_2, "
        AND dt.", date_column, " >= DATEADD(day, ", days_start, ", cp.cohort_start_date)
        AND dt.", date_column, " <= DATEADD(day, ", days_end, ", cp.cohort_start_date)
    )
    SELECT 
      COUNT(DISTINCT cp.subject_id) as total_patients,
      COUNT(DISTINCT pwt.subject_id) as patients_with_test,
      CASE 
        WHEN COUNT(DISTINCT cp.subject_id) > 0 
        THEN CAST(COUNT(DISTINCT pwt.subject_id) AS FLOAT) / COUNT(DISTINCT cp.subject_id) * 100
        ELSE 0 
      END as prevalence_pct
    FROM cohort_patients cp
    LEFT JOIN patients_with_test pwt ON cp.subject_id = pwt.subject_id
  ")
  
  result <- DBI::dbGetQuery(connection, prevalence_sql)
  
  return(list(
    total_patients = result$total_patients[1],
    patients_with_test = result$patients_with_test[1],
    prevalence_pct = result$prevalence_pct[1]
  ))
}

#' Insert Results into Database Table
#'
#' @param connection Database connection
#' @param cohort_database_schema Schema name
#' @param result_table_name Table name
#' @param results_data Data frame with results
insert_results_to_db <- function(connection, cohort_database_schema, result_table_name, results_data) {
  full_table_name <- paste0(cohort_database_schema, ".", result_table_name)
  
  DBI::dbWriteTable(
    conn = connection,
    name = DBI::SQL(full_table_name),
    value = results_data,
    append = TRUE,
    row.names = FALSE
  )
}

#' Create Summary Statistics Table
#'
#' @param connection Database connection
#' @param schema Schema name
#' @param result_table_name Results table name
create_summary_table_db <- function(connection, schema, result_table_name) {
  summary_table_name <- paste0(result_table_name, "_summary")
  full_summary_table <- paste0(schema, ".", summary_table_name)
  full_results_table <- paste0(schema, ".", result_table_name)
  
  # Drop summary table if exists
  tryCatch({
    DBI::dbExecute(connection, paste("DROP TABLE IF EXISTS", full_summary_table))
  }, error = function(e) {})
  
  # Create summary table
  summary_sql <- paste0("
    CREATE TABLE ", full_summary_table, " AS
    SELECT 
      cohortname,
      omop_object_domain,
      workflow_stage,
      COUNT(*) as total_tests,
      AVG(patient_count) as avg_prevalence_pct,
      MIN(patient_count) as min_prevalence_pct,
      MAX(patient_count) as max_prevalence_pct,
      SUM(n_patients_with_op) as total_patients_with_tests,
      AVG(n_patients) as avg_cohort_size,
      GETDATE() as summary_date
    FROM ", full_results_table, "
    GROUP BY cohortname, omop_object_domain, workflow_stage
  ")
  
  DBI::dbExecute(connection, summary_sql)
}