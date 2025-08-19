# Complete Example: Database-Sufficient Prevalence Analysis
# 
# This script demonstrates a complete workflow for prevalence analysis
# using the computeGaps package with database storage.

library(computeGaps)
library(DBI)

# =============================================================================
# STEP 1: Setup Database Connection
# =============================================================================

# Example connection setup (adjust for your environment)
# For SQL Server with Windows Authentication:
connection_string <- create_connection_string(
  server = "your_server_name",
  database = "your_database_name",
  trusted_connection = TRUE
)

# Establish connection (uncomment and modify as needed)
# con <- DBI::dbConnect(odbc::odbc(), .connection_string = connection_string)

# For testing purposes, you can also use:
# con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

# =============================================================================
# STEP 2: Create Sample Data for Testing
# =============================================================================

# Create sample TSV data
sample_file <- "sample_prevalence_data.tsv"
create_sample_tsv_data(sample_file, n_rows = 20)

cat("Sample TSV file created:", sample_file, "\n")
cat("Sample data preview:\n")
sample_data <- readr::read_tsv(sample_file, show_col_types = FALSE)
print(head(sample_data, 3))

# =============================================================================
# STEP 3: Validate Database Setup (uncomment when using real database)
# =============================================================================

# # Validate database access
# validate_db_access(con, "results_schema", "cdm_schema")
# 
# # Check cohort table
# cohort_info <- get_cohort_definitions(con, "results_schema", "your_cohort_table")
# print(cohort_info)

# =============================================================================
# STEP 4: Run Complete Prevalence Analysis
# =============================================================================

# Example with real database (uncomment and modify):
# results <- run_complete_prevalence_analysis(
#   tsv_file_path = sample_file,
#   connection = con,
#   cohort_database_schema = "results_schema",
#   cohort_table_name = "your_cohort_table",
#   cdm_database_schema = "cdm_schema",
#   result_table_name = "prevalence_analysis_example",
#   create_summary = TRUE,
#   create_indexes = TRUE,
#   verbose = TRUE
# )

# For demonstration without database:
cat("\n=== Analysis Configuration ===\n")
cat("TSV file:", sample_file, "\n")
cat("Expected results table: results_schema.prevalence_analysis_example\n")
cat("Expected summary table: results_schema.prevalence_analysis_example_summary\n")

# =============================================================================
# STEP 5: Query and Export Results (when using real database)
# =============================================================================

# # Query all results
# all_results <- query_prevalence_results(
#   con, "results_schema", "prevalence_analysis_example"
# )
# 
# # Query filtered results
# high_prevalence <- query_prevalence_results(
#   con, "results_schema", "prevalence_analysis_example",
#   filters = list(patient_count_min = 5.0),
#   order_by = "patient_count DESC",
#   limit = 50
# )
# 
# # Export results
# export_prevalence_results(
#   con, "results_schema", "prevalence_analysis_example",
#   output_file = "prevalence_results.csv",
#   format = "csv"
# )

# =============================================================================
# STEP 6: Demonstrate Utility Functions
# =============================================================================

cat("\n=== Domain Mapping Examples ===\n")
domains <- c("Procedure", "Measurement", "Condition", "Drug")
for (domain in domains) {
  table_name <- get_domain_table_name(domain)
  date_col <- get_domain_date_column(table_name)
  concept_col <- get_domain_concept_column(table_name)
  
  cat(sprintf("Domain: %s -> Table: %s, Date: %s, Concept: %s\n", 
              domain, table_name, date_col, concept_col))
}

# =============================================================================
# STEP 7: Show Expected Database Schema
# =============================================================================

cat("\n=== Expected Database Tables ===\n")
cat("Results table structure:\n")
cat("- analysis_id (BIGINT, PRIMARY KEY)\n")
cat("- cohortname (VARCHAR(255))\n")
cat("- omop_object_domain (VARCHAR(50))\n")
cat("- object_custom_name (VARCHAR(255))\n")
cat("- workflow_stage (VARCHAR(100))\n")
cat("- patient_count (DECIMAL(10,4)) -- Prevalence percentage\n")
cat("- n_patients (BIGINT) -- Total cohort size\n")
cat("- n_patients_with_op (BIGINT) -- Patients with test/procedure\n")
cat("- concept_id_1, concept_id_2 (BIGINT)\n")
cat("- days_around_index_1, days_around_index_2 (INT)\n")
cat("- analysis_date (DATETIME2)\n")

cat("\nSummary table structure:\n")
cat("- cohortname, omop_object_domain, workflow_stage\n")
cat("- total_tests, avg_prevalence_pct, min_prevalence_pct, max_prevalence_pct\n")
cat("- total_patients_with_tests, avg_cohort_size\n")
cat("- summary_date\n")

# =============================================================================
# STEP 8: Cleanup
# =============================================================================

# Clean up sample file
if (file.exists(sample_file)) {
  unlink(sample_file)
  cat("\nSample file cleaned up.\n")
}

# Close database connection (uncomment when using real database)
# DBI::dbDisconnect(con)

cat("\n=== Example Complete ===\n")
cat("To run with a real database:\n")
cat("1. Uncomment and configure the database connection\n")
cat("2. Update schema and table names\n")
cat("3. Ensure your cohort table exists with proper structure\n")
cat("4. Run the analysis functions\n")
cat("5. Query and export results as needed\n")