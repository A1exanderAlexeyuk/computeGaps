test_that("Domain mapping functions work correctly", {
  # Test domain table mapping
  expect_equal(get_domain_table_name("Procedure"), "procedure_occurrence")
  expect_equal(get_domain_table_name("Measurement"), "measurement")
  expect_equal(get_domain_table_name("Condition"), "condition_occurrence")
  expect_equal(get_domain_table_name("Drug"), "drug_exposure")
  expect_equal(get_domain_table_name("Device"), "device_exposure")
  expect_equal(get_domain_table_name("Observation"), "observation")
  
  # Test invalid domain
  expect_error(get_domain_table_name("InvalidDomain"))
})

test_that("Date column mapping works correctly", {
  expect_equal(get_domain_date_column("procedure_occurrence"), "procedure_date")
  expect_equal(get_domain_date_column("measurement"), "measurement_date")
  expect_equal(get_domain_date_column("condition_occurrence"), "condition_start_date")
  expect_equal(get_domain_date_column("drug_exposure"), "drug_exposure_start_date")
  expect_equal(get_domain_date_column("device_exposure"), "device_exposure_start_date")
  expect_equal(get_domain_date_column("observation"), "observation_date")
  
  # Test invalid table
  expect_error(get_domain_date_column("invalid_table"))
})

test_that("Concept column mapping works correctly", {
  expect_equal(get_domain_concept_column("procedure_occurrence"), "procedure_concept_id")
  expect_equal(get_domain_concept_column("measurement"), "measurement_concept_id")
  expect_equal(get_domain_concept_column("condition_occurrence"), "condition_concept_id")
  expect_equal(get_domain_concept_column("drug_exposure"), "drug_concept_id")
  expect_equal(get_domain_concept_column("device_exposure"), "device_concept_id")
  expect_equal(get_domain_concept_column("observation"), "observation_concept_id")
  
  # Test invalid table
  expect_error(get_domain_concept_column("invalid_table"))
})

test_that("Connection string creation works", {
  # Test trusted connection
  conn_str <- create_connection_string("server1", "database1", TRUE)
  expect_true(grepl("Trusted_Connection=yes", conn_str))
  expect_true(grepl("Server=server1", conn_str))
  expect_true(grepl("Database=database1", conn_str))
  
  # Test username/password connection
  conn_str <- create_connection_string("server1", "database1", FALSE, "user1", "pass1")
  expect_true(grepl("UID=user1", conn_str))
  expect_true(grepl("PWD=pass1", conn_str))
  expect_false(grepl("Trusted_Connection", conn_str))
  
  # Test error when credentials missing
  expect_error(create_connection_string("server1", "database1", FALSE))
})

test_that("SQL platform optimization works", {
  # Test SQL Server optimization
  sql <- "SELECT * FROM table LIMIT 10"
  optimized <- optimize_sql_for_platform(sql, "Microsoft SQL Server")
  expect_true(grepl("TOP 10", optimized))
  expect_false(grepl("LIMIT", optimized))
  
  # Test PostgreSQL optimization
  sql <- "SELECT TOP 10 * FROM table WHERE date = GETDATE()"
  optimized <- optimize_sql_for_platform(sql, "PostgreSQL")
  expect_true(grepl("LIMIT 10", optimized))
  expect_true(grepl("CURRENT_TIMESTAMP", optimized))
  expect_false(grepl("TOP", optimized))
  expect_false(grepl("GETDATE", optimized))
})