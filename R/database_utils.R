#' Database Utility Functions for Prevalence Analysis
#'
#' This file contains utility functions for database operations
#' used in prevalence analysis.

#' Check if Table Exists
#'
#' @param connection Database connection
#' @param schema Schema name
#' @param table_name Table name
#'
#' @return Logical indicating if table exists
#' @keywords internal
table_exists <- function(connection, schema, table_name) {
  tryCatch({
    sql <- "SELECT 1 FROM @schema.@table_name WHERE 1=0"
    sql <- SqlRender::render(sql, schema = schema, table_name = table_name)
    DatabaseConnector::querySql(connection, sql)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Drop Table if Exists
#'
#' @param connection Database connection
#' @param schema Schema name
#' @param table_name Table name
#'
#' @keywords internal
drop_table <- function(connection, schema, table_name) {
  sql <- "DROP TABLE IF EXISTS @schema.@table_name"
  sql <- SqlRender::render(sql, schema = schema, table_name = table_name)
  DatabaseConnector::executeSql(connection, sql)
}

#' Create Results Table
#'
#' @param connection Database connection
#' @param schema Schema name
#' @param table_name Table name
#'
#' @keywords internal
create_results_table <- function(connection, schema, table_name) {
  dbms <- DatabaseConnector::dbms(connection)

  if (dbms == "redshift") {
    sql <- "
CREATE TABLE @schema.@table_name (
    result_id BIGINT IDENTITY(1,1),
    cohortname VARCHAR(255) NOT NULL,
    omop_object_domain VARCHAR(50) NOT NULL,
    object_custom_name VARCHAR(255) NOT NULL,
    workflow_stage VARCHAR(100) NOT NULL,
    concept_id_1 BIGINT NOT NULL,
    concept_id_2 BIGINT NOT NULL,
    n_patients BIGINT NOT NULL,
    n_patients_with_op BIGINT NOT NULL,
    patient_count DECIMAL(5,2) NOT NULL,
    days_around_index_1 INT NOT NULL,
    days_around_index_2 INT NOT NULL,
    analysis_date DATE NOT NULL,
    created_timestamp DATE
)"
  }
  sql <- SqlRender::render(sql, schema = schema, table_name = table_name)
  DatabaseConnector::executeSql(connection, sql)
}

#' Insert Results to Database
#'
#' @param connection Database connection
#' @param cohort_database_schema Schema name
#' @param results_table_name Table name
#' @param results_data Data frame with results
#'
#' @keywords internal
insert_results_to_db <- function(connection, cohort_database_schema, results_table_name, results_data) {
  # Prepare data for insertion
  results_data$created_timestamp <- Sys.time()

  # Use DatabaseConnector to insert data
  DatabaseConnector::insertTable(
    connection = connection,
    tableName = paste0(cohort_database_schema, ".", results_table_name),
    data = results_data,
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = FALSE
  )
}

#' Create Indexes on Results Table
#'
#' @param connection Database connection
#' @param schema Schema name
#' @param table_name Table name
#'
#' @keywords internal
create_results_indexes <- function(connection, schema, table_name) {
  indexes <- list(
    "CREATE INDEX IX_@table_name_cohortname ON @schema.@table_name (cohortname)",
    "CREATE INDEX IX_@table_name_domain ON @schema.@table_name (omop_object_domain)",
    "CREATE INDEX IX_@table_name_workflow ON @schema.@table_name (workflow_stage)",
    "CREATE INDEX IX_@table_name_concepts ON @schema.@table_name (concept_id_1, concept_id_2)",
    "CREATE INDEX IX_@table_name_analysis_date ON @schema.@table_name (analysis_date)"
  )

  for (index_sql in indexes) {
    sql <- SqlRender::render(index_sql, schema = schema, table_name = table_name)
    tryCatch({
      DatabaseConnector::executeSql(connection, sql)
    }, error = function(e) {
      warning("Failed to create index: ", e$message)
    })
  }
}

#' Get Table Row Count
#'
#' @param connection Database connection
#' @param schema Schema name
#' @param table_name Table name
#'
#' @return Integer row count
#' @keywords internal
get_table_row_count <- function(connection, schema, table_name) {
  sql <- "SELECT COUNT(*) as row_count FROM @schema.@table_name"
  sql <- SqlRender::render(sql, schema = schema, table_name = table_name)
  result <- DatabaseConnector::querySql(connection, sql)
  return(result$ROW_COUNT[1])
}

#' Validate Database Connection
#'
#' @param connection Database connection object
#'
#' @return Logical indicating if connection is valid
#' @keywords internal
validate_connection <- function(connection) {
  if (is.null(connection)) {
    stop("Database connection is NULL")
  }

  tryCatch({
    # Test connection with simple query
    DatabaseConnector::querySql(connection, "SELECT 1 as test")
    return(TRUE)
  }, error = function(e) {
    stop("Database connection is not valid: ", e$message)
  })
}

#' Execute SQL with Error Handling
#'
#' @param connection Database connection
#' @param sql SQL query string
#' @param error_message Custom error message
#'
#' @return Query result or NULL if error
#' @keywords internal
execute_sql_safe <- function(connection, sql, error_message = "SQL execution failed") {
  tryCatch({
    return(DatabaseConnector::querySql(connection, sql))
  }, error = function(e) {
    warning(error_message, ": ", e$message)
    return(NULL)
  })
}

#' Get Database Platform
#'
#' @param connection Database connection
#'
#' @return Character string indicating database platform
#' @keywords internal
get_database_platform <- function(connection) {
  tryCatch({
    # Try to determine database type
    if (inherits(connection, "DatabaseConnectorConnection")) {
      return(attr(connection, "dbms"))
    } else if (inherits(connection, "SQLServerConnection")) {
      return("sql server")
    } else if (inherits(connection, "PostgreSQLConnection")) {
      return("postgresql")
    } else if (inherits(connection, "OracleConnection")) {
      return("oracle")
    } else {
      return("unknown")
    }
  }, error = function(e) {
    return("unknown")
  })
}

#' Adapt SQL for Database Platform
#'
#' @param sql SQL query string
#' @param platform Database platform
#'
#' @return Adapted SQL query
#' @keywords internal
adapt_sql_for_platform <- function(sql, platform) {
  if (platform == "postgresql") {
    # Replace SQL Server specific functions
    sql <- gsub("DATEADD\\(day,", "DATE_ADD(INTERVAL", sql)
    sql <- gsub("GETDATE\\(\\)", "CURRENT_TIMESTAMP", sql)
    sql <- gsub("IDENTITY\\(1,1\\)", "SERIAL", sql)
    sql <- gsub("DATETIME2", "TIMESTAMP", sql)
  } else if (platform == "oracle") {
    # Replace SQL Server specific functions for Oracle
    sql <- gsub("DATEADD\\(day, ([^,]+), ([^)]+)\\)", "\\2 + \\1", sql)
    sql <- gsub("GETDATE\\(\\)", "SYSDATE", sql)
    sql <- gsub("IDENTITY\\(1,1\\)", "", sql)
    sql <- gsub("DATETIME2", "TIMESTAMP", sql)
  }

  return(sql)
}
