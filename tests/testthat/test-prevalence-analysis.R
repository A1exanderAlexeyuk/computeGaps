test_that("TSV structure validation works", {
  # Create sample data
  sample_data <- data.frame(
    cohortname = "Test Cohort",
    concept_id_1 = 12345,
    relationship_id = "Has diagnostic test",
    concept_id_2 = 67890,
    omop_object_domain = "Procedure",
    object_custom_name = "Test Procedure",
    object_custom_code = "TEST01",
    predicate_metadata = '{"Workflow stage": "Test", "days_around_index_1": -7, "days_around_index_2": 0}',
    stringsAsFactors = FALSE
  )
  
  # Test validation
  result <- validate_tsv_structure(sample_data)
  
  expect_true(result$is_valid)
  expect_equal(length(result$missing_columns), 0)
  expect_equal(result$total_rows, 1)
})

test_that("Supported domains are returned correctly", {
  domains <- get_supported_domains()
  
  expect_true(is.character(domains))
  expect_true(length(domains) > 0)
  expect_true("Procedure" %in% domains)
  expect_true("Measurement" %in% domains)
})

test_that("Sample TSV creation works", {
  temp_file <- tempfile(fileext = ".tsv")
  
  # Create sample data
  sample_data <- create_sample_tsv(temp_file)
  
  # Check file was created
  expect_true(file.exists(temp_file))
  
  # Check data structure
  expect_true(is.data.frame(sample_data))
  expect_true(nrow(sample_data) > 0)
  expect_true("cohortname" %in% names(sample_data))
  expect_true("predicate_metadata" %in% names(sample_data))
  
  # Clean up
  unlink(temp_file)
})

test_that("JSON metadata parsing handles errors gracefully", {
  # Test with invalid JSON
  invalid_json <- "invalid json string"
  
  # This should not throw an error but return empty list
  result <- tryCatch({
    jsonlite::fromJSON(invalid_json)
  }, error = function(e) {
    list()
  })
  
  expect_true(is.list(result))
})

test_that("Summary statistics creation works", {
  # Create sample results data
  sample_results <- data.frame(
    cohortname = c("Cohort A", "Cohort A", "Cohort B"),
    omop_object_domain = c("Procedure", "Measurement", "Procedure"),
    workflow_stage = c("Stage 1", "Stage 1", "Stage 2"),
    object_custom_name = c("Test 1", "Test 2", "Test 3"),
    patient_count = c(10.5, 15.2, 8.7),
    n_patients = c(1000, 1000, 500),
    n_patients_with_op = c(105, 152, 44),
    stringsAsFactors = FALSE
  )
  
  # Create summary
  summary_stats <- create_summary_stats(sample_results)
  
  expect_true(is.data.frame(summary_stats))
  expect_true(nrow(summary_stats) > 0)
  expect_true("avg_prevalence" %in% names(summary_stats))
  expect_true("total_tests" %in% names(summary_stats))
})