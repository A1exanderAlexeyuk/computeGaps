test_that("Database utility functions work correctly", {
  # Skip tests if no database connection available
  skip_if_not(exists("test_connection"), "No test database connection available")
  
  # Test table existence checking
  # This test assumes a non-existent table
  expect_false(table_exists(test_connection, "test_schema", "non_existent_table"))
})

test_that("SQL building functions work", {
  # Test SQL query building
  sql <- build_prevalence_sql(
    cohort_database_schema = "test_schema",
    cdm_database_schema = "cdm_schema", 
    cohort_table_name = "cohort",
    concept_id_1 = 12345,
    concept_id_2 = 67890,
    domain = "Procedure",
    days_start = -30,
    days_end = 30
  )
  
  expect_type(sql, "character")
  expect_true(grepl("procedure_occurrence", sql))
  expect_true(grepl("procedure_concept_id", sql))
  expect_true(grepl("12345", sql))
  expect_true(grepl("67890", sql))
})

test_that("Database platform detection works", {
  # Test with mock connection objects
  mock_connection <- structure(list(), class = "MockConnection")
  
  # Should return "unknown" for unrecognized connection types
  platform <- get_database_platform(mock_connection)
  expect_equal(platform, "unknown")
})

test_that("SQL adaptation for different platforms works", {
  sql_server_sql <- "SELECT DATEADD(day, 30, GETDATE()) as test_date"
  
  # Test PostgreSQL adaptation
  pg_sql <- adapt_sql_for_platform(sql_server_sql, "postgresql")
  expect_true(grepl("DATE_ADD", pg_sql) || grepl("CURRENT_TIMESTAMP", pg_sql))
  
  # Test Oracle adaptation  
  oracle_sql <- adapt_sql_for_platform(sql_server_sql, "oracle")
  expect_true(grepl("SYSDATE", oracle_sql))
  
  # Test SQL Server (should remain unchanged)
  ss_sql <- adapt_sql_for_platform(sql_server_sql, "sql server")
  expect_equal(ss_sql, sql_server_sql)
})