# Simple Prevalence Calculation Example
# This script demonstrates how to calculate prevalence without database storage

library(computeGaps)
library(dplyr)
library(tibble)

# Example 1: Using your exact data structure
cat("=== Example 1: Using provided data structure ===\n\n")

# Create sample data matching your input format
sample_data <- tibble::tribble(
  ~cohortname, ~concept_id_1, ~relationship_id, ~concept_id_2, ~omop_object_domain, ~object_custom_name, ~object_custom_code, ~predicate_metadata,
  "Reference IBD Ulcerative colitis_InsightsGateway", 81893, "Has diagnostic test", 606840, "Procedure", "Computed tomography of abdomen and pelvis", "DxTest52", '{"Workflow stage": "Confirmatory Diagnosis", "time_gap_in_days": 7, "days_around_index_1": -14, "days_around_index_2": 0}',
  "Reference IBD Ulcerative colitis_InsightsGateway", 81893, "Has diagnostic test", 1091171, "Measurement", "Clostridioides difficile toxin assay", "DxTest4", '{"Workflow stage": "Confirmatory Diagnosis", "time_gap_in_days": 7, "days_around_index_1": -14, "days_around_index_2": 0}'
)

# Calculate prevalence (mock mode - generates random data)
results <- calculate_prevalence_simple(sample_data, mock_mode = TRUE, n_patients_per_cohort = 1500)

# Display results
cat("\nRaw Results:\n")
print(results)

cat("\nFormatted Results:\n")
formatted_results <- format_prevalence_results(results)
print(formatted_results)

# Example 2: Working with a TSV file
cat("\n\n=== Example 2: Working with TSV file ===\n\n")

# Save sample data to TSV
temp_file <- tempfile(fileext = ".tsv")
readr::write_tsv(sample_data, temp_file)
cat("Sample data saved to:", temp_file, "\n")

# Calculate prevalence from TSV file
results_from_file <- calculate_prevalence_simple(temp_file, mock_mode = TRUE)

# Clean up
unlink(temp_file)

# Example 3: Larger dataset with multiple cohorts and workflow stages
cat("\n\n=== Example 3: Larger dataset example ===\n\n")

# Create a more comprehensive example
comprehensive_data <- create_example_tsv_data(n_rows = 20)

# Calculate prevalence
comprehensive_results <- calculate_prevalence_simple(
  comprehensive_data, 
  mock_mode = TRUE,
  n_patients_per_cohort = 2000,
  prevalence_range = c(10, 60)  # Adjust prevalence range
)

# Group by workflow stage and calculate summary
summary_by_stage <- comprehensive_results %>%
  group_by(`Workflow stage`) %>%
  summarise(
    n_tests = n(),
    avg_prevalence = round(mean(patient_count), 2),
    min_prevalence = round(min(patient_count), 2),
    max_prevalence = round(max(patient_count), 2),
    total_patients_tested = sum(n_patients_with_op),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_prevalence))

cat("\nSummary by Workflow Stage:\n")
print(summary_by_stage)

# Group by domain
summary_by_domain <- comprehensive_results %>%
  group_by(omop_object_domain) %>%
  summarise(
    n_tests = n(),
    avg_prevalence = round(mean(patient_count), 2),
    total_patients_tested = sum(n_patients_with_op),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_prevalence))

cat("\nSummary by Domain:\n")
print(summary_by_domain)

# Example 4: Understanding the output columns
cat("\n\n=== Example 4: Understanding the output ===\n\n")

cat("Output column definitions:\n")
cat("- cohortname: The cohort identifier from your input\n")
cat("- omop_object_domain: The OMOP domain (Procedure, Measurement, etc.)\n")
cat("- object_custom_name: The name of the test/procedure\n")
cat("- Workflow stage: The workflow stage from predicate_metadata\n")
cat("- patient_count: The PREVALENCE PERCENTAGE (% of patients with the test)\n")
cat("- n_patients: Total number of patients in the cohort\n")
cat("- n_patients_with_op: Number of patients who had the test/procedure\n")
cat("\nNote: patient_count = (n_patients_with_op / n_patients) * 100\n")

# Example 5: Filtering results
cat("\n\n=== Example 5: Filtering results ===\n\n")

# Filter for high prevalence tests (>30%)
high_prevalence <- comprehensive_results %>%
  filter(patient_count > 30) %>%
  arrange(desc(patient_count))

cat("High prevalence tests (>30%):\n")
print(high_prevalence)

# Filter for specific workflow stage
confirmatory_tests <- comprehensive_results %>%
  filter(`Workflow stage` == "Confirmatory Diagnosis") %>%
  arrange(desc(patient_count))

cat("\nConfirmatory Diagnosis tests:\n")
print(confirmatory_tests)

# Example 6: Exporting results
cat("\n\n=== Example 6: Exporting results ===\n\n")

# Export to TSV
output_file <- "prevalence_results.tsv"
readr::write_tsv(comprehensive_results, output_file)
cat("Results exported to:", output_file, "\n")

# Create a summary report
report_lines <- c(
  "Prevalence Analysis Report",
  paste("Generated:", Sys.time()),
  "",
  "Summary Statistics:",
  paste("- Total analyses:", nrow(comprehensive_results)),
  paste("- Unique cohorts:", length(unique(comprehensive_results$cohortname))),
  paste("- Average prevalence:", round(mean(comprehensive_results$patient_count), 2), "%"),
  paste("- Prevalence range:", round(min(comprehensive_results$patient_count), 2), "-", 
        round(max(comprehensive_results$patient_count), 2), "%"),
  "",
  "Top 5 Most Prevalent Tests:",
  ""
)

top_5 <- comprehensive_results %>%
  arrange(desc(patient_count)) %>%
  head(5)

for (i in 1:nrow(top_5)) {
  report_lines <- c(report_lines,
    paste(i, ". ", top_5$object_custom_name[i], " (", 
          top_5$omop_object_domain[i], "): ", 
          round(top_5$patient_count[i], 1), "%", sep = "")
  )
}

report_file <- "prevalence_report.txt"
writeLines(report_lines, report_file)
cat("Report saved to:", report_file, "\n")

# Clean up files
if (file.exists(output_file)) unlink(output_file)
if (file.exists(report_file)) unlink(report_file)

cat("\n=== Example completed ===\n")