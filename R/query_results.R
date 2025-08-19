#' Query and Export Results Functions
#'
#' Functions for querying and exporting prevalence analysis results
#' from the database.

#' Query Prevalence Results from Database
#'
#' @param connection Database connection object
#' @param cohort_database_schema Character. Schema name where results table is stored
#' @param results_table_name Character. Name of the results table
#' @param cohortname Character. Filter by cohort name (optional)
#' @param workflow_stage Character. Filter by workflow stage (optional)
#' @param domain Character. Filter by OMOP domain (optional)
#' @param min_prevalence Numeric. Minimum prevalence percentage to include (optional)
#' @param order_by Character. Column to order results by (default: "patient_count DESC")
#'
#' @return Data frame with query results
#'
#' @examples
#' \dontrun{
#' # Query all results
#' all_results <- query_prevalence_results(
#'   connection = connection,
#'   cohort_database_schema = "results",
#'   results_table_name = "prevalence_analysis_results"
#' )
#' 
#' # Query specific cohort and workflow stage
#' filtered_results <- query_prevalence_results(
#'   connection = connection,
#'   cohort_database_schema = "results", 
#'   results_table_name = "prevalence_analysis_results",
#'   cohortname = "Reference IBD Ulcerative colitis_InsightsGateway",
#'   workflow_stage = "Confirmatory Diagnosis"
#' )
#' 
#' # Query high prevalence results
#' high_prev_results <- query_prevalence_results(
#'   connection = connection,
#'   cohort_database_schema = "results",
#'   results_table_name = "prevalence_analysis_results", 
#'   min_prevalence = 10.0
#' )
#' }
#'
#' @export
query_prevalence_results <- function(connection,
                                    cohort_database_schema,
                                    results_table_name,
                                    cohortname = NULL,
                                    workflow_stage = NULL,
                                    domain = NULL,
                                    min_prevalence = NULL,
                                    order_by = "patient_count DESC") {
  
  # Validate connection
  validate_connection(connection)
  
  # Build base query
  sql <- "
  SELECT 
    result_id,
    cohortname,
    omop_object_domain,
    object_custom_name,
    workflow_stage,
    concept_id_1,
    concept_id_2,
    n_patients,
    n_patients_with_op,
    patient_count,
    days_around_index_1,
    days_around_index_2,
    analysis_date,
    created_timestamp
  FROM {cohort_database_schema}.{results_table_name}
  WHERE 1=1
  "
  
  # Add filters
  if (!is.null(cohortname)) {
    sql <- paste0(sql, " AND cohortname = '", cohortname, "'")
  }
  
  if (!is.null(workflow_stage)) {
    sql <- paste0(sql, " AND workflow_stage = '", workflow_stage, "'")
  }
  
  if (!is.null(domain)) {
    sql <- paste0(sql, " AND omop_object_domain = '", domain, "'")
  }
  
  if (!is.null(min_prevalence)) {
    sql <- paste0(sql, " AND patient_count >= ", min_prevalence)
  }
  
  # Add ordering
  if (!is.null(order_by)) {
    sql <- paste0(sql, " ORDER BY ", order_by)
  }
  
  # Render SQL
  sql <- SqlRender::render(sql, 
                          cohort_database_schema = cohort_database_schema,
                          results_table_name = results_table_name)
  
  # Execute query
  results <- DatabaseConnector::querySql(connection, sql)
  
  return(results)
}

#' Get Results Summary Statistics
#'
#' @param connection Database connection object
#' @param cohort_database_schema Character. Schema name where results table is stored
#' @param results_table_name Character. Name of the results table
#'
#' @return Data frame with summary statistics
#'
#' @examples
#' \dontrun{
#' summary_stats <- get_results_summary(
#'   connection = connection,
#'   cohort_database_schema = "results",
#'   results_table_name = "prevalence_analysis_results"
#' )
#' }
#'
#' @export
get_results_summary <- function(connection, cohort_database_schema, results_table_name) {
  sql <- "
  SELECT 
    COUNT(*) as total_results,
    COUNT(DISTINCT cohortname) as unique_cohorts,
    COUNT(DISTINCT workflow_stage) as unique_workflow_stages,
    COUNT(DISTINCT omop_object_domain) as unique_domains,
    AVG(patient_count) as avg_prevalence,
    MIN(patient_count) as min_prevalence,
    MAX(patient_count) as max_prevalence,
    SUM(n_patients) as total_patients_analyzed,
    SUM(n_patients_with_op) as total_patients_with_procedures,
    MIN(analysis_date) as earliest_analysis,
    MAX(analysis_date) as latest_analysis
  FROM {cohort_database_schema}.{results_table_name}
  "
  
  sql <- SqlRender::render(sql, 
                          cohort_database_schema = cohort_database_schema,
                          results_table_name = results_table_name)
  
  summary <- DatabaseConnector::querySql(connection, sql)
  return(summary)
}

#' Get Results by Cohort
#'
#' @param connection Database connection object
#' @param cohort_database_schema Character. Schema name where results table is stored
#' @param results_table_name Character. Name of the results table
#'
#' @return Data frame with results grouped by cohort
#'
#' @examples
#' \dontrun{
#' cohort_summary <- get_results_by_cohort(
#'   connection = connection,
#'   cohort_database_schema = "results",
#'   results_table_name = "prevalence_analysis_results"
#' )
#' }
#'
#' @export
get_results_by_cohort <- function(connection, cohort_database_schema, results_table_name) {
  sql <- "
  SELECT 
    cohortname,
    COUNT(*) as num_analyses,
    COUNT(DISTINCT workflow_stage) as num_workflow_stages,
    COUNT(DISTINCT omop_object_domain) as num_domains,
    AVG(patient_count) as avg_prevalence,
    MIN(patient_count) as min_prevalence,
    MAX(patient_count) as max_prevalence,
    MAX(n_patients) as cohort_size,
    SUM(n_patients_with_op) as total_procedures_found
  FROM {cohort_database_schema}.{results_table_name}
  GROUP BY cohortname
  ORDER BY avg_prevalence DESC
  "
  
  sql <- SqlRender::render(sql, 
                          cohort_database_schema = cohort_database_schema,
                          results_table_name = results_table_name)
  
  results <- DatabaseConnector::querySql(connection, sql)
  return(results)
}

#' Get Results by Workflow Stage
#'
#' @param connection Database connection object
#' @param cohort_database_schema Character. Schema name where results table is stored
#' @param results_table_name Character. Name of the results table
#'
#' @return Data frame with results grouped by workflow stage
#'
#' @examples
#' \dontrun{
#' workflow_summary <- get_results_by_workflow_stage(
#'   connection = connection,
#'   cohort_database_schema = "results",
#'   results_table_name = "prevalence_analysis_results"
#' )
#' }
#'
#' @export
get_results_by_workflow_stage <- function(connection, cohort_database_schema, results_table_name) {
  sql <- "
  SELECT 
    workflow_stage,
    COUNT(*) as num_analyses,
    COUNT(DISTINCT cohortname) as num_cohorts,
    COUNT(DISTINCT omop_object_domain) as num_domains,
    AVG(patient_count) as avg_prevalence,
    MIN(patient_count) as min_prevalence,
    MAX(patient_count) as max_prevalence,
    SUM(n_patients_with_op) as total_procedures_found
  FROM {cohort_database_schema}.{results_table_name}
  GROUP BY workflow_stage
  ORDER BY avg_prevalence DESC
  "
  
  sql <- SqlRender::render(sql, 
                          cohort_database_schema = cohort_database_schema,
                          results_table_name = results_table_name)
  
  results <- DatabaseConnector::querySql(connection, sql)
  return(results)
}

#' Export Results to File
#'
#' @param connection Database connection object
#' @param cohort_database_schema Character. Schema name where results table is stored
#' @param results_table_name Character. Name of the results table
#' @param output_path Character. Path for output file
#' @param format Character. Output format ("tsv", "csv", "xlsx")
#' @param include_summary Logical. Whether to include summary statistics
#' @param filters List. Optional filters to apply (cohortname, workflow_stage, domain, min_prevalence)
#'
#' @return Character. Path to created output file
#'
#' @examples
#' \dontrun{
#' # Export all results to TSV
#' output_file <- export_results_to_file(
#'   connection = connection,
#'   cohort_database_schema = "results",
#'   results_table_name = "prevalence_analysis_results",
#'   output_path = "prevalence_results.tsv",
#'   format = "tsv"
#' )
#' 
#' # Export filtered results to Excel with summary
#' output_file <- export_results_to_file(
#'   connection = connection,
#'   cohort_database_schema = "results",
#'   results_table_name = "prevalence_analysis_results",
#'   output_path = "filtered_results.xlsx",
#'   format = "xlsx",
#'   include_summary = TRUE,
#'   filters = list(workflow_stage = "Confirmatory Diagnosis", min_prevalence = 5.0)
#' )
#' }
#'
#' @export
export_results_to_file <- function(connection,
                                  cohort_database_schema,
                                  results_table_name,
                                  output_path,
                                  format = "tsv",
                                  include_summary = TRUE,
                                  filters = NULL) {
  
  # Query results with filters
  results <- query_prevalence_results(
    connection = connection,
    cohort_database_schema = cohort_database_schema,
    results_table_name = results_table_name,
    cohortname = filters$cohortname,
    workflow_stage = filters$workflow_stage,
    domain = filters$domain,
    min_prevalence = filters$min_prevalence
  )
  
  if (nrow(results) == 0) {
    warning("No results found matching the specified filters")
    return(NULL)
  }
  
  # Export based on format
  if (format == "tsv") {
    readr::write_tsv(results, output_path)
    
  } else if (format == "csv") {
    readr::write_csv(results, output_path)
    
  } else if (format == "xlsx") {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Package 'openxlsx' is required for Excel export")
    }
    
    wb <- openxlsx::createWorkbook()
    
    # Add main results sheet
    openxlsx::addWorksheet(wb, "Results")
    openxlsx::writeData(wb, "Results", results)
    
    # Add summary sheet if requested
    if (include_summary) {
      summary_stats <- get_results_summary(connection, cohort_database_schema, results_table_name)
      cohort_summary <- get_results_by_cohort(connection, cohort_database_schema, results_table_name)
      workflow_summary <- get_results_by_workflow_stage(connection, cohort_database_schema, results_table_name)
      
      openxlsx::addWorksheet(wb, "Summary")
      openxlsx::writeData(wb, "Summary", summary_stats, startRow = 1)
      openxlsx::writeData(wb, "Summary", cohort_summary, startRow = 5)
      openxlsx::writeData(wb, "Summary", workflow_summary, startRow = 5 + nrow(cohort_summary) + 3)
    }
    
    openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
    
  } else {
    stop("Unsupported format: ", format, ". Supported formats: tsv, csv, xlsx")
  }
  
  message("Results exported to: ", output_path)
  message("Number of rows exported: ", nrow(results))
  
  return(output_path)
}

#' Delete Results from Database
#'
#' @param connection Database connection object
#' @param cohort_database_schema Character. Schema name where results table is stored
#' @param results_table_name Character. Name of the results table
#' @param filters List. Filters to specify which results to delete
#' @param confirm Logical. Whether to require confirmation (default: TRUE)
#'
#' @return Integer. Number of rows deleted
#'
#' @examples
#' \dontrun{
#' # Delete results for specific cohort
#' deleted_count <- delete_results(
#'   connection = connection,
#'   cohort_database_schema = "results",
#'   results_table_name = "prevalence_analysis_results",
#'   filters = list(cohortname = "Reference IBD Ulcerative colitis_InsightsGateway"),
#'   confirm = TRUE
#' )
#' }
#'
#' @export
delete_results <- function(connection,
                          cohort_database_schema,
                          results_table_name,
                          filters = NULL,
                          confirm = TRUE) {
  
  if (is.null(filters) || length(filters) == 0) {
    stop("Filters must be specified to prevent accidental deletion of all results")
  }
  
  # Build delete query
  sql <- "DELETE FROM {cohort_database_schema}.{results_table_name} WHERE 1=1"
  
  if (!is.null(filters$cohortname)) {
    sql <- paste0(sql, " AND cohortname = '", filters$cohortname, "'")
  }
  
  if (!is.null(filters$workflow_stage)) {
    sql <- paste0(sql, " AND workflow_stage = '", filters$workflow_stage, "'")
  }
  
  if (!is.null(filters$domain)) {
    sql <- paste0(sql, " AND omop_object_domain = '", filters$domain, "'")
  }
  
  if (!is.null(filters$analysis_date)) {
    sql <- paste0(sql, " AND analysis_date = '", filters$analysis_date, "'")
  }
  
  # Render SQL
  sql <- SqlRender::render(sql, 
                          cohort_database_schema = cohort_database_schema,
                          results_table_name = results_table_name)
  
  # Get count of rows to be deleted
  count_sql <- gsub("DELETE FROM", "SELECT COUNT(*) as row_count FROM", sql)
  count_result <- DatabaseConnector::querySql(connection, count_sql)
  rows_to_delete <- count_result$ROW_COUNT[1]
  
  if (rows_to_delete == 0) {
    message("No rows match the specified filters")
    return(0)
  }
  
  # Confirm deletion
  if (confirm) {
    message("This will delete ", rows_to_delete, " rows from ", 
            cohort_database_schema, ".", results_table_name)
    response <- readline(prompt = "Continue? (yes/no): ")
    if (tolower(response) != "yes") {
      message("Deletion cancelled")
      return(0)
    }
  }
  
  # Execute deletion
  DatabaseConnector::executeSql(connection, sql)
  
  message("Successfully deleted ", rows_to_delete, " rows")
  return(rows_to_delete)
}