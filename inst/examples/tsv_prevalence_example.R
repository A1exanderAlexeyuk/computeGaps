# Example: Calculate Prevalence from TSV Specification
# 
# This example demonstrates how to calculate prevalence of diagnostic tests
# and procedures from a TSV specification file.

library(computeGaps)
library(DatabaseConnector)

# =============================================================================
# STEP 1: Create Sample TSV Data
# =============================================================================

# Create a sample TSV file for demonstration
sample_tsv <- "sample_prevalence_spec.tsv"
create_sample_tsv(sample_tsv, n_rows = 10)

cat("Sample TSV created:", sample_tsv, "\n")

# Read and display the sample data
tsv_data <- read_and_validate_tsv(sample_tsv)
cat("\nSample data preview:\n")
print(head(tsv_data, 3))

# =============================================================================
# STEP 2: Validate TSV Structure
# =============================================================================

cat("\nValidating TSV structure...\n")
is_valid <- validate_tsv_structure(tsv_data, verbose = TRUE)

if (!is_valid) {
  stop("TSV validation failed. Please check the file format.")
}

# =============================================================================
# STEP 3: Connect to Database
# =============================================================================

# Example connection (adjust for your database)
# For PostgreSQL:
# conn <- DatabaseConnector::connect(
#   dbms = "postgresql",
#   server = "localhost/omop_cdm",
#   user = "username",
#   password = "password"
# )

# For SQL Server:
# conn <- DatabaseConnector::connect(
#   dbms = "sql server",
#   server = "server_name",
#   database = "database_name",
#   user = "username",
#   password = "password"
# )

cat("\nNote: Database connection required for actual prevalence calculation.\n")

# =============================================================================
# STEP 4: Calculate Prevalence (requires database connection)
# =============================================================================

# Uncomment and run with actual database connection:
# results <- compute_prevalence_from_tsv(
#   conn = conn,
#   cdm_schema = "cdm",
#   cohort_schema = "results",
#   cohort_table = "cohort",
#   tsv_path = sample_tsv,
#   cohort_definition_id = 12345  # optional: filter to specific cohort
# )

# Example of expected output structure:
example_results <- tibble::tibble(
  cohortname = c("IBD UC", "IBD UC", "IBD Crohn"),
  omop_object_domain = c("Procedure", "Measurement", "Procedure"),
  object_custom_name = c("CT abdomen", "C-reactive protein", "Colonoscopy"),
  `Workflow stage` = c("Confirmatory Diagnosis", "Initial Assessment", "Follow-up"),
  patient_count = c(156, 89, 234),
  n_patients = c(1000, 1000, 850),
  n_patients_with_op = c(156, 89, 234)
)

cat("\nExample output structure:\n")
print(example_results)

# =============================================================================
# STEP 5: Analyze Results
# =============================================================================

# Create summary statistics
cat("\nSummary statistics:\n")
summary_stats <- create_summary_stats(example_results)
print(summary_stats)

# =============================================================================
# STEP 6: Export Results
# =============================================================================

# Export to TSV
export_results(example_results, output_prefix = "example_prevalence", formats = "tsv")

# Create text report
create_prevalence_report(example_results, output_path = "example_report.txt")

cat("\nFiles created:\n")
cat("- example_prevalence.tsv\n")
cat("- example_report.txt\n")

# =============================================================================
# STEP 7: Clean Up
# =============================================================================

# Remove example files
unlink(sample_tsv)
unlink("example_prevalence.tsv")
unlink("example_report.txt")

cat("\nExample complete. Temporary files cleaned up.\n")

# =============================================================================
# NOTES ON TSV FORMAT
# =============================================================================

cat("\n=== TSV Format Notes ===\n")
cat("Required columns:\n")
cat("- cohortname: Cohort identifier\n")
cat("- concept_id_1: Source concept (for reference)\n")
cat("- concept_id_2: Target concept to calculate prevalence\n")
cat("- omop_object_domain: OMOP domain (Procedure, Measurement, etc.)\n")
cat("- object_custom_name: Human-readable name\n")
cat("- predicate_metadata: JSON with time window\n")
cat("\nJSON metadata format:\n")
cat('{"Workflow stage": "Stage Name", "days_around_index_1": -14, "days_around_index_2": 0}\n')
cat("\nTime window is relative to cohort_start_date:\n")
cat("- Negative days = before index\n")
cat("- Positive days = after index\n")