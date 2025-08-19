# Performance Example: Efficient Batch Processing
# This example demonstrates how computeGaps efficiently handles large TSV files

library(computeGaps)
library(DatabaseConnector)
library(tibble)

# Create a large example TSV with 1000 rows across multiple domains
create_large_example_tsv <- function(file_path, n_tests_per_domain = 200) {
  
  domains <- c("Procedure", "Measurement", "Condition", "Drug", "Observation")
  cohorts <- c("IBD UC", "IBD Crohn", "RA Cohort")
  stages <- c("Initial Diagnosis", "Confirmatory Diagnosis", "Follow-up")
  
  # Generate random concept IDs and test names
  rows <- list()
  
  for (domain in domains) {
    for (i in 1:n_tests_per_domain) {
      cohort <- sample(cohorts, 1)
      stage <- sample(stages, 1)
      
      # Random time windows
      days1 <- sample(c(-30, -14, -7, 0), 1)
      days2 <- sample(c(0, 7, 14, 30), 1)
      
      row <- tibble(
        cohortname = cohort,
        concept_id_1 = sample(c(81893, 201606, 80809), 1),  # UC, Crohn's, RA
        relationship_id = "Has diagnostic test",
        concept_id_2 = 1000000 + sample(1:999999, 1),  # Random concept IDs
        omop_object_domain = domain,
        object_custom_name = paste(domain, "Test", i),
        object_custom_code = paste0(substr(domain, 1, 3), "Test", i),
        predicate_metadata = sprintf(
          '{"Workflow stage": "%s", "days_around_index_1": %d, "days_around_index_2": %d}',
          stage, days1, days2
        )
      )
      
      rows[[length(rows) + 1]] <- row
    }
  }
  
  large_df <- dplyr::bind_rows(rows)
  readr::write_tsv(large_df, file_path)
  
  return(large_df)
}

# Example usage
cat("Creating large example TSV with 1000 rows...\n")
large_tsv <- create_large_example_tsv("large_analysis.tsv", n_tests_per_domain = 200)

cat("\nDataset summary:\n")
cat("Total rows:", nrow(large_tsv), "\n")
cat("Unique domains:", length(unique(large_tsv$omop_object_domain)), "\n")
cat("Unique concepts:", length(unique(large_tsv$concept_id_2)), "\n")

# Show how many queries will be executed
domain_summary <- large_tsv %>%
  dplyr::group_by(omop_object_domain) %>%
  dplyr::summarise(
    n_tests = n(),
    n_unique_concepts = length(unique(concept_id_2)),
    n_unique_windows = length(unique(paste(days_around_index_1, days_around_index_2)))
  )

cat("\nEfficiency demonstration:\n")
cat("Traditional approach: Would execute", nrow(large_tsv), "separate SQL queries\n")
cat("computeGaps approach: Will execute only", length(unique(large_tsv$omop_object_domain)), "SQL queries (one per domain)\n")
cat("\nQuery reduction factor:", round(nrow(large_tsv) / length(unique(large_tsv$omop_object_domain))), "x fewer queries!\n")

print(domain_summary)

# Run the analysis (when connected to a database)
# connection <- DatabaseConnector::connect(...)
# 
# start_time <- Sys.time()
# results <- compute_prevalence_from_tsv(
#   conn = connection,
#   cdm_schema = "cdm",
#   cohort_schema = "results", 
#   cohort_table = "cohort",
#   tsv_path = "large_analysis.tsv",
#   cohort_definition_id = 81893
# )
# end_time <- Sys.time()
# 
# cat("\nAnalysis completed in:", difftime(end_time, start_time, units = "secs"), "seconds\n")
# cat("Results rows:", nrow(results), "\n")

# Clean up
if (file.exists("large_analysis.tsv")) {
  unlink("large_analysis.tsv")
}

cat("\n=== Performance Benefits ===\n")
cat("1. Reduced database round trips\n")
cat("2. Better query optimization by the database engine\n")
cat("3. Reduced network overhead\n")
cat("4. Faster overall execution time\n")
cat("5. Lower database server load\n")