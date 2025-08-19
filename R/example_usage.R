#' Example Usage of Prevalence Analysis Functions
#'
#' This script demonstrates how to use the prevalence analysis functions
#' with sample data and database connections.

# Load required libraries
library(dplyr)
library(readr)
library(jsonlite)
library(DatabaseConnector)

#' Example function to demonstrate the complete workflow with database storage
#'
#' @param tsv_file_path Character. Path to TSV file
#' @param db_connection Database connection object
#' @param cohort_table Character. Name of cohort table
#' @param output_dir Character. Directory for output files
#' @param store_results Logical. Whether to store results in database
#' @param results_table Character. Name of results table in database
#'
#' @export
run_complete_prevalence_analysis <- function(tsv_file_path, 
                                           db_connection, 
                                           cohort_table,
                                           output_dir = "output",
                                           store_results = TRUE,
                                           results_table = "prevalence_results") {
  
  cat("=== STARTING COMPLETE PREVALENCE ANALYSIS WORKFLOW ===\n")
  
  # Step 1: Validate inputs
  cat("Step 1: Validating inputs...\n")
  
  if (!file.exists(tsv_file_path)) {
    stop("TSV file not found: ", tsv_file_path)
  }
  
  if (!validate_connection(db_connection)) {
    stop("Database connection is not valid")
  }
  
  # Step 2: Read and validate TSV data
  cat("Step 2: Reading and validating TSV data...\n")
  
  tsv_data <- readr::read_tsv(tsv_file_path, show_col_types = FALSE)
  validation_result <- validate_tsv_structure(tsv_data)
  
  if (!validation_result$is_valid) {
    stop("TSV validation failed. Missing columns: ", 
         paste(validation_result$missing_columns, collapse = ", "))
  }
  
  cat("TSV validation passed. Found", validation_result$total_rows, "rows.\n")
  
  if (length(validation_result$json_errors) > 0) {
    cat("Warning: JSON parsing errors in rows:", 
        paste(validation_result$json_errors, collapse = ", "), "\n")
  }
  
  # Step 3: Run prevalence analysis
  cat("Step 3: Running prevalence analysis...\n")
  
  results <- analyze_prevalence_from_tsv(
    tsv_file_path = tsv_file_path,
    connection = db_connection,
    cohort_table_name = cohort_table
  )
  
  # Step 4: Store results in database if requested
  if (store_results) {
    cat("Step 4: Storing results in database...\n")
    
    store_prevalence_results(
      results = results,
      connection = db_connection,
      table_name = results_table,
      overwrite = TRUE
    )
    
    cat("Results stored in table:", results_table, "\n")
  }
  
  # Step 5: Create output directory and export results
  cat("Step 5: Exporting results to files...\n")
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  output_prefix <- file.path(output_dir, "prevalence_analysis_results")
  export_results(results, output_prefix, formats = c("tsv", "csv", "json"))
  
  # Step 6: Create comprehensive report
  cat("Step 6: Creating comprehensive report...\n")
  
  report_path <- file.path(output_dir, "prevalence_comprehensive_report.html")
  create_comprehensive_report(results, report_path, db_connection, cohort_table)
  
  # Step 7: Generate summary statistics
  cat("Step 7: Generating summary statistics...\n")
  
  summary_stats <- create_detailed_summary(results)
  summary_path <- file.path(output_dir, "summary_statistics.json")
  write_json(summary_stats, summary_path, pretty = TRUE)
  
  # Step 8: Display final summary
  cat("Step 8: Analysis summary...\n")
  print(summary_stats)
  
  cat("\n=== COMPLETE ANALYSIS WORKFLOW FINISHED ===\n")
  cat("Results exported to:", output_dir, "\n")
  if (store_results) {
    cat("Results stored in database table:", results_table, "\n")
  }
  
  return(list(
    results = results,
    summary = summary_stats,
    output_dir = output_dir,
    database_table = if(store_results) results_table else NULL
  ))
}

#' Quick Test Function with Database Integration
#'
#' @param use_memory_db Logical. Whether to use in-memory SQLite database
#' @param db_path Character. Path to database file if not using memory
#'
#' @export
quick_prevalence_test <- function(use_memory_db = TRUE, db_path = "test_prevalence.db") {
  cat("=== QUICK PREVALENCE ANALYSIS TEST ===\n")
  
  # Step 1: Create test database and sample data
  cat("Step 1: Setting up test environment...\n")
  
  if (use_memory_db) {
    connectionDetails <- DatabaseConnector::createConnectionDetails(
      dbms = "sqlite",
      server = ":memory:"
    )
  } else {
    connectionDetails <- DatabaseConnector::createConnectionDetails(
      dbms = "sqlite",
      server = db_path
    )
  }
  
  con <- DatabaseConnector::connect(connectionDetails)
  
  # Create sample TSV data
  sample_file <- tempfile(fileext = ".tsv")
  sample_data <- create_comprehensive_sample_tsv(sample_file)
  
  # Create sample cohort table
  cohort_table <- "test_cohort"
  create_test_cohort_table(con, cohort_table)
  
  # Step 2: Run analysis
  cat("Step 2: Running prevalence analysis...\n")
  
  results <- run_complete_prevalence_analysis(
    tsv_file_path = sample_file,
    db_connection = con,
    cohort_table = cohort_table,
    output_dir = tempdir(),
    store_results = TRUE,
    results_table = "test_prevalence_results"
  )
  
  # Step 3: Verify database storage
  cat("Step 3: Verifying database storage...\n")
  
  stored_count <- DatabaseConnector::querySql(con, "SELECT COUNT(*) as count FROM test_prevalence_results")
  cat("Records stored in database:", stored_count$COUNT, "\n")
  
  # Step 4: Test query functions
  cat("Step 4: Testing query functions...\n")
  
  test_query_results <- query_prevalence_by_cohort(con, "test_prevalence_results", "Reference IBD")
  cat("Query results for IBD cohorts:", nrow(test_query_results), "records\n")
  
  # Step 5: Clean up
  cat("Step 5: Cleaning up...\n")
  
  DatabaseConnector::disconnect(con)
  if (file.exists(sample_file)) {
    file.remove(sample_file)
  }
  if (!use_memory_db && file.exists(db_path)) {
    file.remove(db_path)
  }
  
  cat("Quick test completed successfully!\n")
  cat("Analysis processed", nrow(results$results), "prevalence calculations\n")
  
  return(results)
}

#' Demonstrate Query Functions
#'
#' @param db_connection Database connection object
#' @param results_table Character. Name of results table
#'
#' @export
demonstrate_prevalence_queries <- function(db_connection, results_table = "prevalence_results") {
  cat("=== DEMONSTRATING PREVALENCE QUERY FUNCTIONS ===\n")
  
  # Query 1: Get all results for specific cohort pattern
  cat("Query 1: Results for IBD cohorts...\n")
  ibd_results <- query_prevalence_by_cohort(db_connection, results_table, "IBD")
  cat("Found", nrow(ibd_results), "IBD-related prevalence records\n")
  
  # Query 2: Get high prevalence items (>50%)
  cat("Query 2: High prevalence items (>50%)...\n")
  high_prev <- query_high_prevalence(db_connection, results_table, threshold = 0.5)
  cat("Found", nrow(high_prev), "high prevalence items\n")
  
  # Query 3: Get results by domain
  cat("Query 3: Results by domain...\n")
  procedure_results <- query_prevalence_by_domain(db_connection, results_table, "Procedure")
  cat("Found", nrow(procedure_results), "procedure-related prevalence records\n")
  
  # Query 4: Get prevalence trends over time
  cat("Query 4: Prevalence summary statistics...\n")
  summary_stats <- query_prevalence_summary(db_connection, results_table)
  print(summary_stats)
  
  # Query 5: Get results by workflow stage
  cat("Query 5: Results by workflow stage...\n")
  workflow_results <- query_prevalence_by_workflow_stage(db_connection, results_table, "Initial Assessment")
  cat("Found", nrow(workflow_results), "Initial Assessment stage records\n")
  
  cat("Query demonstration completed!\n")
  
  return(list(
    ibd_results = ibd_results,
    high_prevalence = high_prev,
    procedure_results = procedure_results,
    summary_stats = summary_stats,
    workflow_results = workflow_results
  ))
}

#' Create Comprehensive Sample TSV Data for Testing
#'
#' @param output_path Character. Path where to save the sample TSV file
#'
#' @export
create_comprehensive_sample_tsv <- function(output_path = "comprehensive_sample_data.tsv") {
  
  sample_data <- data.frame(
    cohortname = c(
      "Reference IBD Ulcerative colitis_InsightsGateway",
      "Reference IBD Ulcerative colitis_InsightsGateway",
      "Reference IBD Crohn's disease_InsightsGateway",
      "Reference IBD Crohn's disease_InsightsGateway",
      "Reference Diabetes Type 2_InsightsGateway",
      "Reference Diabetes Type 2_InsightsGateway",
      "Reference Hypertension_InsightsGateway",
      "Reference Hypertension_InsightsGateway"
    ),
    concept_id_1 = c(81893, 81893, 201606, 201606, 201820, 201820, 316866, 316866),
    relationship_id = c(
      "Has diagnostic test", "Has diagnostic test", "Has diagnostic test", "Has diagnostic test",
      "Has diagnostic test", "Has laboratory test", "Has diagnostic test", "Has laboratory test"
    ),
    concept_id_2 = c(606840, 1091171, 606840, 4024659, 4184637, 3004501, 4273629, 3027018),
    omop_object_domain = c(
      "Procedure", "Measurement", "Procedure", "Measurement",
      "Measurement", "Measurement", "Procedure", "Measurement"
    ),
    object_custom_name = c(
      "Computed tomography of abdomen and pelvis",
      "Clostridioides difficile toxin assay",
      "Computed tomography of abdomen and pelvis",
      "C-reactive protein measurement",
      "Hemoglobin A1c measurement",
      "Glucose measurement",
      "Echocardiography",
      "Blood pressure measurement"
    ),
    object_custom_code = c(
      "DxTest52", "DxTest4", "DxTest52", "DxTest8",
      "LabTest1", "LabTest2", "DxTest10", "LabTest5"
    ),
    predicate_metadata = c(
      '{"Workflow stage": "Confirmatory Diagnosis", "time_gap_in_days": 7, "days_around_index_1": -14, "days_around_index_2": 0}',
      '{"Workflow stage": "Confirmatory Diagnosis", "time_gap_in_days": 7, "days_around_index_1": -14, "days_around_index_2": 0}',
      '{"Workflow stage": "Initial Assessment", "time_gap_in_days": 14, "days_around_index_1": -30, "days_around_index_2": 7}',
      '{"Workflow stage": "Monitoring", "time_gap_in_days": 30, "days_around_index_1": 0, "days_around_index_2": 90}',
      '{"Workflow stage": "Monitoring", "time_gap_in_days": 90, "days_around_index_1": -30, "days_around_index_2": 30}',
      '{"Workflow stage": "Regular Screening", "time_gap_in_days": 180, "days_around_index_1": -90, "days_around_index_2": 90}',
      '{"Workflow stage": "Initial Assessment", "time_gap_in_days": 30, "days_around_index_1": -15, "days_around_index_2": 15}',
      '{"Workflow stage": "Monitoring", "time_gap_in_days": 30, "days_around_index_1": 0, "days_around_index_2": 30}'
    ),
    stringsAsFactors = FALSE
  )
  
  readr::write_tsv(sample_data, output_path)
  cat("Comprehensive sample TSV data created at:", output_path, "\n")
  cat("Sample includes", nrow(sample_data), "test relationships across multiple conditions\n")
  
  return(sample_data)
}

#' Create Test Database with Sample Cohort Data
#'
#' @param db_connection Database connection object
#' @param cohort_table_name Character. Name for the cohort table
#'
#' @export
create_test_cohort_table <- function(db_connection, cohort_table_name = "test_cohort") {
  
  # Create cohort table
  create_query <- paste0("
    CREATE TABLE IF NOT EXISTS ", cohort_table_name, " (
      cohort_definition_id INTEGER,
      subject_id INTEGER,
      cohort_start_date DATE,
      cohort_end_date DATE,
      PRIMARY KEY (cohort_definition_id, subject_id)
    )"
  )
  
  DatabaseConnector::executeSql(db_connection, create_query)
  
  # Generate sample cohort data
  set.seed(123)  # For reproducible results
  
  cohort_data <- data.frame(
    cohort_definition_id = rep(c(1, 2, 3, 4), each = 1000),  # 4 cohorts, 1000 patients each
    subject_id = c(
      sample(1:10000, 1000),      # UC patients
      sample(10001:20000, 1000),  # Crohn's patients  
      sample(20001:30000, 1000),  # Diabetes patients
      sample(30001:40000, 1000)   # Hypertension patients
    ),
    cohort_start_date = rep(seq(as.Date("2020-01-01"), as.Date("2023-12-31"), length.out = 1000), 4),
    cohort_end_date = rep(seq(as.Date("2021-01-01"), as.Date("2024-12-31"), length.out = 1000), 4)
  )
  
  # Insert data into table
  DatabaseConnector::insertTable(
    connection = db_connection,
    tableName = cohort_table_name,
    data = cohort_data,
    dropTableIfExists = FALSE,
    createTable = FALSE
  )
  
  # Create indexes for better performance
  index_queries <- c(
    paste0("CREATE INDEX IF NOT EXISTS idx_", cohort_table_name, "_cohort_id ON ", cohort_table_name, "(cohort_definition_id)"),
    paste0("CREATE INDEX IF NOT EXISTS idx_", cohort_table_name, "_subject_id ON ", cohort_table_name, "(subject_id)")
  )
  
  for (query in index_queries) {
    DatabaseConnector::executeSql(db_connection, query)
  }
  
  cat("Test cohort table created with", nrow(cohort_data), "patient records\n")
  cat("Cohort definitions: 1=UC, 2=Crohn's, 3=Diabetes, 4=Hypertension\n")
  
  return(cohort_data)
}

#' Setup Complete Test Database Environment
#'
#' @param db_path Character. Path for SQLite database file
#' @param include_sample_data Logical. Whether to include sample analysis data
#'
#' @export
setup_test_database <- function(db_path = "prevalence_test_database.db", include_sample_data = TRUE) {
  
  cat("=== SETTING UP TEST DATABASE ENVIRONMENT ===\n")
  
  # Create database connection
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = db_path
  )
  con <- DatabaseConnector::connect(connectionDetails)
  
  # Create cohort table
  cat("Creating test cohort table...\n")
  cohort_data <- create_test_cohort_table(con, "test_cohort")
  
  if (include_sample_data) {
    # Create sample TSV and run analysis
    cat("Creating sample TSV data and running analysis...\n")
    
    sample_file <- tempfile(fileext = ".tsv")
    create_comprehensive_sample_tsv(sample_file)
    
    # Run prevalence analysis and store results
    results <- run_complete_prevalence_analysis(
      tsv_file_path = sample_file,
      db_connection = con,
      cohort_table = "test_cohort",
      output_dir = dirname(db_path),
      store_results = TRUE,
      results_table = "sample_prevalence_results"
    )
    
    # Clean up temporary file
    file.remove(sample_file)
    
    cat("Sample prevalence analysis completed and stored\n")
  }
  
  # Create additional helper tables
  cat("Creating helper tables...\n")
  
  # Cohort definitions table
  cohort_definitions <- data.frame(
    cohort_definition_id = 1:4,
    cohort_name = c(
      "Reference IBD Ulcerative colitis_InsightsGateway",
      "Reference IBD Crohn's disease_InsightsGateway", 
      "Reference Diabetes Type 2_InsightsGateway",
      "Reference Hypertension_InsightsGateway"
    ),
    description = c(
      "Ulcerative Colitis patient cohort",
      "Crohn's Disease patient cohort",
      "Type 2 Diabetes patient cohort", 
      "Hypertension patient cohort"
    )
  )
  
  DatabaseConnector::insertTable(
    connection = con,
    tableName = "cohort_definitions",
    data = cohort_definitions,
    dropTableIfExists = TRUE,
    createTable = TRUE
  )
  
  # Display database summary
  cat("\n=== DATABASE SETUP COMPLETE ===\n")
  cat("Database file:", db_path, "\n")
  cat("Tables created:\n")
  
  # Get list of tables using a query
  tables_query <- "SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%'"
  tables_df <- DatabaseConnector::querySql(con, tables_query)
  tables <- tables_df$NAME
  
  for (table in tables) {
    count_query <- paste0("SELECT COUNT(*) as count FROM ", table)
    count_result <- DatabaseConnector::querySql(con, count_query)
    cat("  -", table, ":", count_result$COUNT, "records\n")
  }
  
  DatabaseConnector::disconnect(con)
  
  cat("\nDatabase setup completed successfully!\n")
  cat("Use DatabaseConnector::connect(DatabaseConnector::createConnectionDetails(dbms='sqlite', server='", db_path, "')) to connect\n")
  
  return(db_path)
}