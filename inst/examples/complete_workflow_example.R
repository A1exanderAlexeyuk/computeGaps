# Complete Workflow Example for computeGaps Package
# This script demonstrates the full functionality of the prevalence analysis package

# Load required libraries
library(computeGaps)
library(DatabaseConnector)

# =============================================================================
# SETUP DATABASE CONNECTION
# =============================================================================

# Example connection setup (adjust for your database)
# SQL Server
connection <- DatabaseConnector::connect(
  dbms = "sql server",
  server = "your_server_name",
  database = "your_database_name",
  user = "your_username",
  password = "your_password"
)

# PostgreSQL alternative
# connection <- DatabaseConnector::connect(
#   dbms = "postgresql", 
#   server = "your_server/your_database",
#   user = "your_username",
#   password = "your_password"
# )

# =============================================================================
# CONFIGURATION
# =============================================================================

# Database schemas
cohort_database_schema <- "results_schema"  # Where cohort table and results will be stored
cdm_database_schema <- "cdm_schema"         # Where CDM tables are located
cohort_table_name <- "cohort"               # Name of your cohort table

# File paths
tsv_file_path <- "analysis_data.tsv"        # Path to your TSV file
output_directory <- "prevalence_results"    # Directory for output files

# =============================================================================
# STEP 1: CREATE SAMPLE DATA (if needed)
# =============================================================================

# Create sample TSV file if it doesn't exist
if (!file.exists(tsv_file_path)) {
  message("Creating sample TSV data...")
  create_sample_tsv(tsv_file_path, n_rows = 10)
  message("Sample TSV created: ", tsv_file_path)
}

# =============================================================================
# STEP 2: VALIDATE INPUT DATA
# =============================================================================

message("Validating TSV structure...")
tsv_data <- read_and_validate_tsv(tsv_file_path)
validation_result <- validate_tsv_structure(tsv_data, verbose = TRUE)

if (!validation_result) {
  stop("TSV validation failed. Please fix the issues and try again.")
}

message("✓ TSV validation passed")

# =============================================================================
# STEP 3: RUN PREVALENCE ANALYSIS
# =============================================================================

message("Starting prevalence analysis...")

# Option A: Basic analysis
results_table_name <- analyze_prevalence_from_tsv_db(
  tsv_file_path = tsv_file_path,
  connection = connection,
  cohort_database_schema = cohort_database_schema,
  cohort_table_name = cohort_table_name,
  cdm_database_schema = cdm_database_schema,
  results_table_name = "prevalence_analysis_results",
  overwrite_results = TRUE,
  verbose = TRUE
)

message("✓ Analysis completed. Results stored in: ", results_table_name)

# Option B: Complete workflow with automatic reporting
# analysis_results <- run_complete_prevalence_analysis(
#   tsv_file_path = tsv_file_path,
#   connection = connection,
#   cohort_database_schema = cohort_database_schema,
#   cohort_table_name = cohort_table_name,
#   cdm_database_schema = cdm_database_schema,
#   output_dir = output_directory
# )

# =============================================================================
# STEP 4: QUERY AND EXPLORE RESULTS
# =============================================================================

message("Querying and exploring results...")

# Get all results
all_results <- query_prevalence_results(
  connection = connection,
  cohort_database_schema = cohort_database_schema,
  results_table_name = "prevalence_analysis_results"
)

message("Total results found: ", nrow(all_results))

# Get summary statistics
summary_stats <- get_results_summary(
  connection = connection,
  cohort_database_schema = cohort_database_schema,
  results_table_name = "prevalence_analysis_results"
)

print("=== SUMMARY STATISTICS ===")
print(summary_stats)

# Get results by cohort
cohort_summary <- get_results_by_cohort(
  connection = connection,
  cohort_database_schema = cohort_database_schema,
  results_table_name = "prevalence_analysis_results"
)

print("=== RESULTS BY COHORT ===")
print(cohort_summary)

# Get results by workflow stage
workflow_summary <- get_results_by_workflow_stage(
  connection = connection,
  cohort_database_schema = cohort_database_schema,
  results_table_name = "prevalence_analysis_results"
)

print("=== RESULTS BY WORKFLOW STAGE ===")
print(workflow_summary)

# =============================================================================
# STEP 5: FILTER RESULTS
# =============================================================================

message("Demonstrating result filtering...")

# Filter by high prevalence (>= 10%)
high_prevalence_results <- query_prevalence_results(
  connection = connection,
  cohort_database_schema = cohort_database_schema,
  results_table_name = "prevalence_analysis_results",
  min_prevalence = 10.0
)

message("High prevalence results (>=10%): ", nrow(high_prevalence_results))

# Filter by specific workflow stage
confirmatory_results <- query_prevalence_results(
  connection = connection,
  cohort_database_schema = cohort_database_schema,
  results_table_name = "prevalence_analysis_results",
  workflow_stage = "Confirmatory Diagnosis"
)

message("Confirmatory diagnosis results: ", nrow(confirmatory_results))

# Filter by domain
procedure_results <- query_prevalence_results(
  connection = connection,
  cohort_database_schema = cohort_database_schema,
  results_table_name = "prevalence_analysis_results",
  domain = "Procedure"
)

message("Procedure domain results: ", nrow(procedure_results))

# =============================================================================
# STEP 6: EXPORT RESULTS
# =============================================================================

message("Exporting results to files...")

# Create output directory
if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)
}

# Export all results to TSV
export_results_to_file(
  connection = connection,
  cohort_database_schema = cohort_database_schema,
  results_table_name = "prevalence_analysis_results",
  output_path = file.path(output_directory, "all_prevalence_results.tsv"),
  format = "tsv"
)

# Export all results to CSV
export_results_to_file(
  connection = connection,
  cohort_database_schema = cohort_database_schema,
  results_table_name = "prevalence_analysis_results",
  output_path = file.path(output_directory, "all_prevalence_results.csv"),
  format = "csv"
)

# Export to Excel with summary statistics
export_results_to_file(
  connection = connection,
  cohort_database_schema = cohort_database_schema,
  results_table_name = "prevalence_analysis_results",
  output_path = file.path(output_directory, "prevalence_results_with_summary.xlsx"),
  format = "xlsx",
  include_summary = TRUE
)

# Export filtered results (high prevalence only)
if (nrow(high_prevalence_results) > 0) {
  export_results_to_file(
    connection = connection,
    cohort_database_schema = cohort_database_schema,
    results_table_name = "prevalence_analysis_results",
    output_path = file.path(output_directory, "high_prevalence_results.tsv"),
    format = "tsv",
    filters = list(min_prevalence = 10.0)
  )
}

message("✓ Results exported to: ", output_directory)

# =============================================================================
# STEP 7: DEMONSTRATE ADVANCED FEATURES
# =============================================================================

message("Demonstrating advanced query features...")

# Run query demonstration
query_examples <- demonstrate_query_functions(
  connection = connection,
  cohort_database_schema = cohort_database_schema,
  results_table_name = "prevalence_analysis_results"
)

# =============================================================================
# STEP 8: CLEANUP (OPTIONAL)
# =============================================================================

# Uncomment the following lines if you want to clean up test data
# message("Cleaning up test data...")
# delete_results(
#   connection = connection,
#   cohort_database_schema = cohort_database_schema,
#   results_table_name = "prevalence_analysis_results",
#   filters = list(analysis_date = Sys.Date()),
#   confirm = FALSE
# )

# =============================================================================
# STEP 9: DISCONNECT
# =============================================================================

# Close database connection
DatabaseConnector::disconnect(connection)

message("=== WORKFLOW COMPLETE ===")
message("Check the output directory for exported files: ", output_directory)

# =============================================================================
# ADDITIONAL EXAMPLES
# =============================================================================

# Example: Running analysis for specific cohort only
# filtered_tsv_data <- tsv_data[tsv_data$cohortname == "Reference IBD Ulcerative colitis_InsightsGateway", ]
# write_tsv(filtered_tsv_data, "filtered_analysis.tsv")
# 
# results_filtered <- analyze_prevalence_from_tsv_db(
#   tsv_file_path = "filtered_analysis.tsv",
#   connection = connection,
#   cohort_database_schema = cohort_database_schema,
#   results_table_name = "filtered_prevalence_results"
# )

# Example: Custom time windows
# You can modify the predicate_metadata in your TSV file to use different time windows:
# {"Workflow stage": "Custom Analysis", "days_around_index_1": -90, "days_around_index_2": 30}

# Example: Multiple workflow stages
# The same test can be analyzed across different workflow stages by having multiple rows
# in the TSV with different predicate_metadata values

message("See the TSV file format and modify predicate_metadata for custom time windows and workflow stages")