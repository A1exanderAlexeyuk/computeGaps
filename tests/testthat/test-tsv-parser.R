test_that("TSV parsing works correctly", {
  # Create temporary TSV file
  temp_file <- tempfile(fileext = ".tsv")
  
  # Create sample data
  sample_data <- data.frame(
    cohortname = "Test Cohort",
    concept_id_1 = 12345,
    relationship_id = "Has diagnostic test",
    concept_id_2 = 67890,
    omop_object_domain = "Procedure",
    object_custom_name = "Test Procedure",
    object_custom_code = "TEST001",
    predicate_metadata = '{"Workflow stage": "Test Stage", "days_around_index_1": -30, "days_around_index_2": 30}',
    stringsAsFactors = FALSE
  )
  
  readr::write_tsv(sample_data, temp_file)
  
  # Test reading and validation
  expect_silent(tsv_data <- read_and_validate_tsv(temp_file))
  expect_equal(nrow(tsv_data), 1)
  expect_equal(tsv_data$cohortname, "Test Cohort")
  
  # Test metadata parsing
  expect_silent(parsed_data <- parse_analysis_metadata(tsv_data))
  expect_equal(parsed_data$workflow_stage, "Test Stage")
  expect_equal(parsed_data$days_around_index_1, -30)
  expect_equal(parsed_data$days_around_index_2, 30)
  
  # Clean up
  unlink(temp_file)
})

test_that("TSV validation catches errors", {
  # Create temporary TSV file with missing columns
  temp_file <- tempfile(fileext = ".tsv")
  
  # Create invalid data (missing required columns)
  invalid_data <- data.frame(
    cohortname = "Test Cohort",
    concept_id_1 = 12345,
    stringsAsFactors = FALSE
  )
  
  readr::write_tsv(invalid_data, temp_file)
  
  # Test that validation fails
  expect_error(read_and_validate_tsv(temp_file))
  
  # Clean up
  unlink(temp_file)
})

test_that("JSON metadata parsing handles errors gracefully", {
  # Test invalid JSON
  invalid_json <- "invalid json string"
  result <- parse_predicate_metadata(invalid_json)
  
  expect_equal(result$workflow_stage, "Unknown")
  expect_equal(result$days_around_index_1, -30)
  expect_equal(result$days_around_index_2, 30)
})

test_that("Sample TSV creation works", {
  temp_file <- tempfile(fileext = ".tsv")
  
  # Create sample TSV
  expect_silent(create_sample_tsv(temp_file, n_rows = 3))
  expect_true(file.exists(temp_file))
  
  # Validate created file
  tsv_data <- readr::read_tsv(temp_file, show_col_types = FALSE)
  expect_equal(nrow(tsv_data), 3)
  expect_true("cohortname" %in% names(tsv_data))
  expect_true("predicate_metadata" %in% names(tsv_data))
  
  # Clean up
  unlink(temp_file)
})