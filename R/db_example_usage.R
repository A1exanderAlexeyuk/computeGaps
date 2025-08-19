#' Database-Sufficient Example Usage and Wrapper Functions
#'
#' This file contains example usage and high-level wrapper functions for 
#' database-sufficient prevalence analysis.

#' Complete Database-Sufficient Prevalence Analysis Workflow
#'
#' This function provides a complete workflow for prevalence analysis that:
#' 1. Reads TSV data
#' 2. Validates database access
#' 3. Performs prevalence analysis
#' 4. Stores results in database
#' 5. Creates summary reports
#'
#' @param tsv_file_path Character. Path to TSV file
#' @param connection Database connection object
#' @param cohort_database_schema Character. Schema for cohort and results tables
#' @param cohort_table_name Character. Cohort table name
#' @param cdm_database_schema Character. CDM schema (optional, defaults to cohort_database_schema)
#' @param result_table_name Character. Results table name (optional)
#' @param create_summary Logical. Whether to create summary tables (default: TRUE)
#' @param create_indexes Logical. Whether to create performance indexes (default: TRUE)
#' @param cleanup_on_error Logical. Whether to cleanup on error (default: TRUE)
#' @param verbose Logical. Whether to print progress (default: TRUE)
#'
#' @return List with analysis results and metadata
#' @export
#'
#' @examples
#' \dontrun{
#' # Complete workflow
#' results <- run_complete_prevalence_analysis(
#'   tsv_file_path = "data/ibd_tests.tsv",
#'   connection = con,
#'   cohort_database_schema = "results",
#'   cohort_table_name = "ibd_cohort",
#'   cdm_database_schema = "cdm"
#' )
#' 
#' # Access results
#' cat("Results table:", results$result_table_name, "\n")
#' cat("Summary table:", results$summary_table_name, "\n")
#' print(results$analysis_summary)
#' }
run_complete_prevalence_analysis <- function(tsv_file_path,
                                           connection,
                                           cohort_database_schema,
                                           cohort_table_name,
                                           cdm_database_schema = NULL,
                                           result_table_name = NULL,
                                           create_summary = TRUE,
                                           create_indexes = TRUE,
                                           cleanup_on_error = TRUE,
                                           verbose = TRUE) {
  
  start_time <- Sys.time()
  
  tryCatch({
    # Set defaults
    if (is.null(cdm_database_schema)) {
      cdm_database_schema <- cohort_database_schema
    }
    
    if (is.null(result_table_name)) {
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      result_table_name <- paste0("prevalence_analysis_", timestamp)
    }
    
    if (verbose) {
      cat("=== Database-Sufficient Prevalence Analysis ===\n")
      cat("Start time:", format(start_time), "\n")
      cat("TSV file:", tsv_file_path, "\n")
      cat("Cohort schema:", cohort_database_schema, "\n")
      cat("CDM schema:", cdm_database_schema, "\n")
      cat("Results table:", result_table_name, "\n\n")
    }
    
    # Step 1: Validate database access
    if (verbose) cat("Step 1: Validating database access...\n")
    validate_db_access(connection, cohort_database_schema, cdm_database_schema)
    
    # Step 2: Check cohort table exists
    if (verbose) cat("Step 2: Validating cohort table...\n")
    if (!table_exists_in_schema(connection, cohort_database_schema, cohort_table_name)) {
      stop("Cohort table not found: ", cohort_database_schema, ".", cohort_table_name)
    }
    
    # Step 3: Get cohort information
    if (verbose) cat("Step 3: Getting cohort information...\n")
    cohort_info <- get_cohort_definitions(connection, cohort_database_schema, cohort_table_name)
    if (verbose) {
      cat("Found", nrow(cohort_info), "cohort definition(s)\n")
      print(cohort_info)
    }
    
    # Step 4: Run prevalence analysis
    if (verbose) cat("Step 4: Running prevalence analysis...\n")
    result_table_full_name <- analyze_prevalence_db_sufficient(
      tsv_file_path = tsv_file_path,
      connection = connection,
      cohort_database_schema = cohort_database_schema,
      cohort_table_name = cohort_table_name,
      result_table_name = result_table_name,
      cdm_database_schema = cdm_database_schema,
      verbose = verbose
    )
    
    # Step 5: Create performance indexes
    if (create_indexes) {
      if (verbose) cat("Step 5: Creating performance indexes...\n")
      create_performance_indexes(connection, cohort_database_schema, result_table_name)
    }
    
    # Step 6: Generate analysis summary
    if (verbose) cat("Step 6: Generating analysis summary...\n")
    analysis_summary <- generate_analysis_summary(connection, cohort_database_schema, result_table_name)
    
    # Step 7: Create summary tables if requested
    summary_table_name <- NULL
    if (create_summary) {
      if (verbose) cat("Step 7: Creating summary tables...\n")
      summary_table_name <- paste0(result_table_name, "_summary")
      create_summary_table_db(connection, cohort_database_schema, result_table_name)
    }
    
    end_time <- Sys.time()
    duration <- as.numeric(difftime(end_time, start_time, units = "mins"))
    
    if (verbose) {
      cat("\n=== Analysis Completed Successfully ===\n")
      cat("End time:", format(end_time), "\n")
      cat("Duration:", round(duration, 2), "minutes\n")
      cat("Results table:", result_table_full_name, "\n")
      if (!is.null(summary_table_name)) {
        cat("Summary table:", cohort_database_schema, ".", summary_table_name, "\n")
      }
    }
    
    # Return results
    return(list(
      success = TRUE,
      result_table_name = result_table_full_name,
      summary_table_name = if (!is.null(summary_table_name)) paste0(cohort_database_schema, ".", summary_table_name) else NULL,
      analysis_summary = analysis_summary,
      cohort_info = cohort_info,
      start_time = start_time,
      end_time = end_time,
      duration_minutes = duration
    ))
    
  }, error = function(e) {
    if (cleanup_on_error) {
      if (verbose) cat("Error occurred, cleaning up...\n")
      tryCatch({
        cleanup_temp_tables(connection, cohort_database_schema, paste0(result_table_name, "%"))
      }, error = function(cleanup_error) {
        warning("Cleanup failed: ", cleanup_error$message)
      })
    }
    
    stop("Prevalence analysis failed: ", e$message)
  })
}

#' Create Performance Indexes on Results Table
#'
#' @param connection Database connection
#' @param schema Schema name
#' @param table_name Table name
create_performance_indexes <- function(connection, schema, table_name) {
  # Index for cohort and domain queries
  create_results_index(connection, schema, table_name, 
                      c("cohortname", "omop_object_domain"), 
                      paste0("IX_", table_name, "_cohort_domain"))
  
  # Index for workflow stage queries
  create_results_index(connection, schema, table_name, 
                      c("workflow_stage"), 
                      paste0("IX_", table_name, "_workflow"))
  
  # Index for concept queries
  create_results_index(connection, schema, table_name, 
                      c("concept_id_1", "concept_id_2"), 
                      paste0("IX_", table_name, "_concepts"))
  
  # Index for prevalence queries
  create_results_index(connection, schema, table_name, 
                      c("patient_count"), 
                      paste0("IX_", table_name, "_prevalence"))
}

#' Generate Analysis Summary Statistics
#'
#' @param connection Database connection
#' @param schema Schema name
#' @param table_name Results table name
#' @return Data frame with summary statistics
generate_analysis_summary <- function(connection, schema, table_name) {
  full_table_name <- paste0(schema, ".", table_name)
  
  summary_sql <- paste0("
    SELECT 
      COUNT(*) as total_analyses,
      COUNT(DISTINCT cohortname) as unique_cohorts,
      COUNT(DISTINCT omop_object_domain) as unique_domains,
      COUNT(DISTINCT workflow_stage) as unique_workflow_stages,
      COUNT(DISTINCT object_custom_name) as unique_tests,
      AVG(patient_count) as avg_prevalence_pct,
      MIN(patient_count) as min_prevalence_pct,
      MAX(patient_count) as max_prevalence_pct,
      SUM(n_patients_with_op) as total_patients_with_tests,
      AVG(n_patients) as avg_cohort_size,
      MIN(analysis_date) as first_analysis,
      MAX(analysis_date) as last_analysis
    FROM ", full_table_name
  )
  
  summary <- DBI::dbGetQuery(connection, summary_sql)
  return(summary)
}

#' Query Results from Database
#'
#' @param connection Database connection
#' @param cohort_database_schema Schema name
#' @param result_table_name Results table name
#' @param filters List of filters to apply (optional)
#' @param order_by Character. Column to order by (default: "patient_count DESC")
#' @param limit Integer. Maximum rows to return (optional)
#'
#' @return Data frame with filtered results
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all results
#' all_results <- query_prevalence_results(con, "results", "prevalence_analysis_20241201")
#' 
#' # Get results for specific cohort
#' ibd_results <- query_prevalence_results(
#'   con, "results", "prevalence_analysis_20241201",
#'   filters = list(cohortname = "IBD_Cohort"),
#'   limit = 100
#' )
#' 
#' # Get high prevalence tests
#' high_prev <- query_prevalence_results(
#'   con, "results", "prevalence_analysis_20241201",
#'   filters = list(patient_count_min = 10.0),
#'   order_by = "patient_count DESC"
#' )
#' }
query_prevalence_results <- function(connection, 
                                   cohort_database_schema, 
                                   result_table_name,
                                   filters = NULL,
                                   order_by = "patient_count DESC",
                                   limit = NULL) {
  
  full_table_name <- paste0(cohort_database_schema, ".", result_table_name)
  
  # Build base query
  sql <- paste0("SELECT * FROM ", full_table_name)
  
  # Add filters
  where_conditions <- c()
  
  if (!is.null(filters)) {
    for (filter_name in names(filters)) {
      filter_value <- filters[[filter_name]]
      
      if (filter_name == "cohortname") {
        where_conditions <- c(where_conditions, paste0("cohortname = '", filter_value, "'"))
      } else if (filter_name == "omop_object_domain") {
        where_conditions <- c(where_conditions, paste0("omop_object_domain = '", filter_value, "'"))
      } else if (filter_name == "workflow_stage") {
        where_conditions <- c(where_conditions, paste0("workflow_stage = '", filter_value, "'"))
      } else if (filter_name == "patient_count_min") {
        where_conditions <- c(where_conditions, paste0("patient_count >= ", filter_value))
      } else if (filter_name == "patient_count_max") {
        where_conditions <- c(where_conditions, paste0("patient_count <= ", filter_value))
      } else if (filter_name == "concept_id_1") {
        where_conditions <- c(where_conditions, paste0("concept_id_1 = ", filter_value))
      } else if (filter_name == "concept_id_2") {
        where_conditions <- c(where_conditions, paste0("concept_id_2 = ", filter_value))
      }
    }
  }
  
  if (length(where_conditions) > 0) {
    sql <- paste0(sql, " WHERE ", paste(where_conditions, collapse = " AND "))
  }
  
  # Add ordering
  if (!is.null(order_by)) {
    sql <- paste0(sql, " ORDER BY ", order_by)
  }
  
  # Add limit
  if (!is.null(limit)) {
    platform <- get_db_platform(connection)
    if (tolower(platform) %in% c("microsoft sql server", "sql server")) {
      sql <- paste0("SELECT TOP ", limit, " * FROM (", sql, ") subquery")
    } else {
      sql <- paste0(sql, " LIMIT ", limit)
    }
  }
  
  return(DBI::dbGetQuery(connection, sql))
}

#' Export Results to File
#'
#' @param connection Database connection
#' @param cohort_database_schema Schema name
#' @param result_table_name Results table name
#' @param output_file Character. Output file path
#' @param format Character. Output format ("csv", "tsv", "xlsx")
#' @param filters List of filters (optional)
#'
#' @export
export_prevalence_results <- function(connection,
                                     cohort_database_schema,
                                     result_table_name,
                                     output_file,
                                     format = "csv",
                                     filters = NULL) {
  
  # Query results
  results <- query_prevalence_results(connection, cohort_database_schema, result_table_name, filters)
  
  # Export based on format
  if (format == "csv") {
    readr::write_csv(results, output_file)
  } else if (format == "tsv") {
    readr::write_tsv(results, output_file)
  } else if (format == "xlsx") {
    if (requireNamespace("openxlsx", quietly = TRUE)) {
      openxlsx::write.xlsx(results, output_file)
    } else {
      stop("openxlsx package required for Excel export")
    }
  } else {
    stop("Unsupported format: ", format)
  }
  
  cat("Results exported to:", output_file, "\n")
  cat("Rows exported:", nrow(results), "\n")
}

#' Create Sample TSV Data for Testing
#'
#' @param output_file Character. Output TSV file path
#' @param n_rows Integer. Number of sample rows to create
#' @export
create_sample_tsv_data <- function(output_file, n_rows = 10) {
  sample_data <- data.frame(
    cohortname = rep(c("Reference IBD Ulcerative colitis_InsightsGateway", "IBD_Crohns_Disease"), length.out = n_rows),
    concept_id_1 = rep(c(81893, 201606), length.out = n_rows),
    relationship_id = rep("Has diagnostic test", n_rows),
    concept_id_2 = c(606840, 1091171, 4298794, 4035726, 4244107, 4035411, 4035412, 4035413, 4035414, 4035415)[1:n_rows],
    omop_object_domain = rep(c("Procedure", "Measurement"), length.out = n_rows),
    object_custom_name = c(
      "Computed tomography of abdomen and pelvis",
      "Clostridioides difficile toxin assay",
      "Complete blood count",
      "C-reactive protein",
      "Erythrocyte sedimentation rate",
      "Fecal calprotectin",
      "Colonoscopy",
      "Flexible sigmoidoscopy",
      "Upper endoscopy",
      "Capsule endoscopy"
    )[1:n_rows],
    object_custom_code = paste0("DxTest", 1:n_rows),
    predicate_metadata = rep(c(
      '{"Workflow stage": "Confirmatory Diagnosis", "time_gap_in_days": 7, "days_around_index_1": -14, "days_around_index_2": 0}',
      '{"Workflow stage": "Initial Assessment", "time_gap_in_days": 30, "days_around_index_1": -30, "days_around_index_2": 30}'
    ), length.out = n_rows),
    stringsAsFactors = FALSE
  )
  
  readr::write_tsv(sample_data, output_file)
  cat("Sample TSV data created:", output_file, "\n")
  cat("Rows created:", nrow(sample_data), "\n")
}