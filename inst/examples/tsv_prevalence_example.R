# Example: TSV-based Prevalence Calculation
# This script demonstrates how to calculate prevalence from a TSV file

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
cdm_schema <- "cdm_schema"           # Where CDM tables are located
cohort_schema <- "results_schema"    # Where cohort table is located
cohort_table <- "cohort"             # Name of your cohort table

# File paths
tsv_file_path <- "analysis_data.tsv" # Path to your TSV file

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
validation_result <- validate_tsv_structure(tsv_data)

if (!validation_result$is_valid) {
  stop("TSV validation failed. Missing columns: ", 
       paste(validation_result$missing_columns, collapse = ", "))
}

message("✓ TSV validation passed")
message("  Total rows: ", validation_result$total_rows)
if (length(validation_result$json_errors) > 0) {
  warning("  JSON parsing errors in rows: ", 
          paste(validation_result$json_errors, collapse = ", "))
}

# =============================================================================
# STEP 3: RUN PREVALENCE ANALYSIS
# =============================================================================

message("Starting prevalence analysis...")

# Run analysis - returns a tibble
results <- compute_prevalence_from_tsv(
  conn = connection,
  cdm_schema = cdm_schema,
  cohort_schema = cohort_schema,
  cohort_table = cohort_table,
  tsv_path = tsv_file_path,
  cohort_definition_id = NULL  # Set to specific ID if needed
)

message("✓ Analysis completed. Processed ", nrow(results), " combinations")

# =============================================================================
# STEP 4: EXPLORE RESULTS
# =============================================================================

message("\n=== RESULTS PREVIEW ===")
print(head(results, 10))

# Summary by workflow stage
message("\n=== SUMMARY BY WORKFLOW STAGE ===")
summary_by_stage <- results %>%
  dplyr::group_by(`Workflow stage`) %>%
  dplyr::summarise(
    n_tests = dplyr::n(),
    avg_prevalence = round(mean(patient_count, na.rm = TRUE), 2),
    .groups = "drop"
  )
print(summary_by_stage)

# Summary by domain
message("\n=== SUMMARY BY DOMAIN ===")
summary_by_domain <- results %>%
  dplyr::group_by(omop_object_domain) %>%
  dplyr::summarise(
    n_tests = dplyr::n(),
    avg_prevalence = round(mean(patient_count, na.rm = TRUE), 2),
    .groups = "drop"
  )
print(summary_by_domain)

# =============================================================================
# STEP 5: CREATE SUMMARY STATISTICS
# =============================================================================

message("\n=== DETAILED SUMMARY STATISTICS ===")
summary_stats <- create_summary_stats(results)
print(summary_stats)

# =============================================================================
# STEP 6: EXPORT RESULTS
# =============================================================================

# Export results to TSV
export_results(results, output_prefix = "prevalence_results", formats = c("tsv"))

# Export to multiple formats if needed
# export_results(results, output_prefix = "prevalence_results", formats = c("tsv", "csv", "xlsx"))

# =============================================================================
# STEP 7: CREATE REPORT
# =============================================================================

create_prevalence_report(results, output_path = "prevalence_report.txt")
message("✓ Report created: prevalence_report.txt")

# =============================================================================
# STEP 8: FILTER RESULTS (EXAMPLES)
# =============================================================================

# High prevalence results (>= 10%)
high_prevalence <- results %>%
  dplyr::filter(patient_count >= 10)
message("\nHigh prevalence results (>=10%): ", nrow(high_prevalence), " tests")

# Specific workflow stage
confirmatory <- results %>%
  dplyr::filter(`Workflow stage` == "Confirmatory Diagnosis")
message("Confirmatory diagnosis results: ", nrow(confirmatory), " tests")

# Specific domain
procedures <- results %>%
  dplyr::filter(omop_object_domain == "Procedure")
message("Procedure results: ", nrow(procedures), " tests")

# =============================================================================
# STEP 9: DISCONNECT
# =============================================================================

# Close database connection
DatabaseConnector::disconnect(connection)

message("\n=== ANALYSIS COMPLETE ===")
message("Results saved to: prevalence_results.tsv")
message("Report saved to: prevalence_report.txt")