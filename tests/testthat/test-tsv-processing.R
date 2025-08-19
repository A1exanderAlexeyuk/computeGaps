test_that("TSV structure validation works", {
  # Create valid TSV data
  valid_tsv <- data.frame(
    cohortname = "Test Cohort",
    concept_id_1 = 12345,
    relationship_id = "Has diagnostic test",
    concept_id_2 = 67890,
    omop_object_domain = "Procedure",
    object_custom_name = "Test Procedure",
    object_custom_code = "TEST01",
    predicate_metadata = '{"Workflow stage": "Test", "days_around_index_1": -14, "days_around_index_2": 0}',
    stringsAsFactors = FALSE
  )
  
  # Should not throw error
  expect_silent(validate_tsv_structure_db(valid_tsv))
  
  # Test missing columns
  invalid_tsv <- valid_tsv[, -1]  # Remove first column
  expect_error(validate_tsv_structure_db(invalid_tsv), "Missing required columns")
  
  # Test invalid domain
  invalid_domain_tsv <- valid_tsv
  invalid_domain_tsv$omop_object_domain <- "InvalidDomain"
  expect_error(validate_tsv_structure_db(invalid_domain_tsv), "Unsupported OMOP domains")
  
  # Test non-numeric concept IDs
  invalid_concept_tsv <- valid_tsv
  invalid_concept_tsv$concept_id_1 <- "not_numeric"
  expect_error(validate_tsv_structure_db(invalid_concept_tsv), "must be numeric")
})

test_that("Metadata parsing works correctly", {
  # Create test data with JSON metadata
  test_data <- data.frame(
    cohortname = "Test Cohort",
    concept_id_1 = 12345,
    relationship_id = "Has diagnostic test",
    concept_id_2 = 67890,
    omop_object_domain = "Procedure",
    object_custom_name = "Test Procedure",
    object_custom_code = "TEST01",
    predicate_metadata = c(
      '{"Workflow stage": "Confirmatory Diagnosis", "time_gap_in_days": 7, "days_around_index_1": -14, "days_around_index_2": 0}',
      '{"Workflow stage": "Initial Assessment", "time_gap_in_days": 30, "days_around_index_1": -30, "days_around_index_2": 30}'
    ),
    stringsAsFactors = FALSE
  )
  
  # Parse metadata
  parsed_data <- parse_tsv_metadata_db(test_data)
  
  # Check parsed fields
  expect_equal(parsed_data$workflow_stage[1], "Confirmatory Diagnosis")
  expect_equal(parsed_data$workflow_stage[2], "Initial Assessment")
  expect_equal(parsed_data$time_gap_days[1], 7)
  expect_equal(parsed_data$time_gap_days[2], 30)
  expect_equal(parsed_data$days_around_index_1[1], -14)
  expect_equal(parsed_data$days_around_index_1[2], -30)
  expect_equal(parsed_data$days_around_index_2[1], 0)
  expect_equal(parsed_data$days_around_index_2[2], 30)
})

test_that("Sample TSV creation works", {
  temp_file <- tempfile(fileext = ".tsv")
  
  # Create sample data
  create_sample_tsv_data(temp_file, n_rows = 5)
  
  # Check file exists
  expect_true(file.exists(temp_file))
  
  # Read and validate structure
  sample_data <- readr::read_tsv(temp_file, show_col_types = FALSE)
  expect_equal(nrow(sample_data), 5)
  expect_silent(validate_tsv_structure_db(sample_data))
  
  # Clean up
  unlink(temp_file)
})