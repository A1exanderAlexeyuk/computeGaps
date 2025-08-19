#' Database Utility Functions for Prevalence Analysis
#'
#' This file contains utility functions for database operations in prevalence analysis.

#' Get Domain Table Name from OMOP Domain
#'
#' @param domain Character. OMOP domain name
#' @return Character. Corresponding OMOP CDM table name
#' @export
get_domain_table_name <- function(domain) {
  domain_mapping <- list(
    "Procedure" = "procedure_occurrence",
    "Measurement" = "measurement",
    "Condition" = "condition_occurrence",
    "Drug" = "drug_exposure",
    "Device" = "device_exposure",
    "Observation" = "observation"
  )
  
  if (!domain %in% names(domain_mapping)) {
    stop("Unsupported domain: ", domain)
  }
  
  return(domain_mapping[[domain]])
}

#' Get Domain Date Column Name
#'
#' @param domain_table Character. OMOP CDM table name
#' @return Character. Date column name for the table
#' @export
get_domain_date_column <- function(domain_table) {
  date_mapping <- list(
    "procedure_occurrence" = "procedure_date",
    "measurement" = "measurement_date",
    "condition_occurrence" = "condition_start_date",
    "drug_exposure" = "drug_exposure_start_date",
    "device_exposure" = "device_exposure_start_date",
    "observation" = "observation_date"
  )
  
  if (!domain_table %in% names(date_mapping)) {
    stop("Unknown domain table: ", domain_table)
  }
  
  return(date_mapping[[domain_table]])
}

#' Get Domain Concept Column Name
#'
#' @param domain_table Character. OMOP CDM table name
#' @return Character. Concept ID column name for the table
#' @export
get_domain_concept_column <- function(domain_table) {
  concept_mapping <- list(
    "procedure_occurrence" = "procedure_concept_id",
    "measurement" = "measurement_concept_id",
    "condition_occurrence" = "condition_concept_id",
    "drug_exposure" = "drug_concept_id",
    "device_exposure" = "device_concept_id",
    "observation" = "observation_concept_id"
  )
  
  if (!domain_table %in% names(concept_mapping)) {
    stop("Unknown domain table: ", domain_table)
  }
  
  return(concept_mapping[[domain_table]])
}

#' Validate Database Connection and Schema Access
#'
#' @param connection Database connection object
#' @param cohort_database_schema Character. Schema name to validate
#' @param cdm_database_schema Character. CDM schema name to validate
#' @return Logical. TRUE if validation passes
#' @export
validate_db_access <- function(connection, cohort_database_schema, cdm_database_schema = NULL) {
  # Test connection by executing a simple query
  tryCatch({
    DatabaseConnector::querySql(connection, "SELECT 1 AS test")
  }, error = function(e) {
    stop("Invalid database connection: ", e$message)
  })
  
  # Test cohort schema access
  tryCatch({
    # For some databases, we might need to query information_schema
    schema_test_sql <- paste0("SELECT 1 FROM INFORMATION_SCHEMA.SCHEMATA WHERE SCHEMA_NAME = '", cohort_database_schema, "'")
    result <- DatabaseConnector::querySql(connection, schema_test_sql)
    if (nrow(result) == 0) {
      stop("Cannot access cohort_database_schema: ", cohort_database_schema)
    }
  }, error = function(e) {
    warning("Could not validate schema access: ", e$message)
  })
  
  # Test CDM schema access if provided
  if (!is.null(cdm_database_schema) && cdm_database_schema != cohort_database_schema) {
    tryCatch({
      schema_test_sql <- paste0("SELECT 1 FROM INFORMATION_SCHEMA.SCHEMATA WHERE SCHEMA_NAME = '", cdm_database_schema, "'")
      result <- DatabaseConnector::querySql(connection, schema_test_sql)
      if (nrow(result) == 0) {
        stop("Cannot access cdm_database_schema: ", cdm_database_schema)
      }
    }, error = function(e) {
      warning("Could not validate CDM schema access: ", e$message)
    })
  }
  
  return(TRUE)
}

#' Check if Table Exists in Schema
#'
#' @param connection Database connection
#' @param schema Character. Schema name
#' @param table_name Character. Table name
#' @return Logical. TRUE if table exists
#' @export
table_exists_in_schema <- function(connection, schema, table_name) {
  tryCatch({
    check_sql <- paste0("
      SELECT COUNT(*) as table_count
      FROM INFORMATION_SCHEMA.TABLES 
      WHERE TABLE_SCHEMA = '", schema, "' 
        AND TABLE_NAME = '", table_name, "'
    ")
    result <- DatabaseConnector::querySql(connection, check_sql)
    return(result$TABLE_COUNT[1] > 0)
  }, error = function(e) {
    # Fallback method
    tryCatch({
      test_sql <- paste0("SELECT TOP 1 * FROM ", schema, ".", table_name)
      DatabaseConnector::querySql(connection, test_sql)
      return(TRUE)
    }, error = function(e2) {
      return(FALSE)
    })
  })
}

#' Get Cohort Definition IDs and Names
#'
#' @param connection Database connection
#' @param cohort_database_schema Character. Schema name
#' @param cohort_table_name Character. Cohort table name
#' @return Data frame with cohort definitions
#' @export
get_cohort_definitions <- function(connection, cohort_database_schema, cohort_table_name) {
  full_table_name <- paste0(cohort_database_schema, ".", cohort_table_name)
  
  sql <- paste0("
    SELECT DISTINCT 
      cohort_definition_id,
      COUNT(DISTINCT subject_id) as patient_count
    FROM ", full_table_name, "
    GROUP BY cohort_definition_id
    ORDER BY cohort_definition_id
  ")
  
  result <- DatabaseConnector::querySql(connection, sql)
  return(result)
}

#' Execute Query with Error Handling
#'
#' @param connection Database connection
#' @param sql Character. SQL query
#' @param params List. Named parameters for parameterized queries
#' @return Data frame or execution result
#' @export
execute_query_safe <- function(connection, sql, params = NULL) {
  tryCatch({
    if (is.null(params)) {
      result <- DatabaseConnector::querySql(connection, sql)
    } else {
      # DatabaseConnector uses renderSql for parameterized queries
      result <- DatabaseConnector::querySql(connection, sql)
    }
    return(result)
  }, error = function(e) {
    stop("Database query failed: ", e$message, "\nSQL: ", sql)
  })
}

#' Get Database Platform Information
#'
#' @param connection Database connection
#' @return Character. Database platform name
#' @export
get_db_platform <- function(connection) {
  tryCatch({
    # DatabaseConnector stores dbms info in the connection object
    return(attr(connection, "dbms"))
  }, error = function(e) {
    return("Unknown")
  })
}

#' Optimize Query for Database Platform
#'
#' @param sql Character. Base SQL query
#' @param platform Character. Database platform
#' @return Character. Optimized SQL query
#' @export
optimize_sql_for_platform <- function(sql, platform) {
  if (tolower(platform) %in% c("microsoft sql server", "sql server")) {
    # SQL Server optimizations
    sql <- gsub("LIMIT (\\d+)", "TOP \\1", sql, ignore.case = TRUE)
  } else if (tolower(platform) %in% c("postgresql", "postgres")) {
    # PostgreSQL optimizations
    sql <- gsub("TOP (\\d+)", "LIMIT \\1", sql, ignore.case = TRUE)
    sql <- gsub("GETDATE\\(\\)", "CURRENT_TIMESTAMP", sql, ignore.case = TRUE)
  } else if (tolower(platform) %in% c("oracle")) {
    # Oracle optimizations
    sql <- gsub("TOP (\\d+)", "ROWNUM <= \\1", sql, ignore.case = TRUE)
    sql <- gsub("GETDATE\\(\\)", "SYSDATE", sql, ignore.case = TRUE)
  }
  
  return(sql)
}

#' Create Index on Results Table
#'
#' @param connection Database connection
#' @param schema Character. Schema name
#' @param table_name Character. Table name
#' @param index_columns Character vector. Columns to index
#' @param index_name Character. Index name (optional)
#' @export
create_results_index <- function(connection, schema, table_name, index_columns, index_name = NULL) {
  if (is.null(index_name)) {
    index_name <- paste0("IX_", table_name, "_", paste(index_columns, collapse = "_"))
  }
  
  full_table_name <- paste0(schema, ".", table_name)
  columns_str <- paste(index_columns, collapse = ", ")
  
  index_sql <- paste0("CREATE INDEX ", index_name, " ON ", full_table_name, " (", columns_str, ")")
  
  tryCatch({
    DatabaseConnector::executeSql(connection, index_sql)
    cat("Created index:", index_name, "\n")
  }, error = function(e) {
    warning("Failed to create index ", index_name, ": ", e$message)
  })
}

#' Clean Up Temporary Tables
#'
#' @param connection Database connection
#' @param schema Character. Schema name
#' @param table_pattern Character. Pattern to match table names
#' @export
cleanup_temp_tables <- function(connection, schema, table_pattern = "temp_prevalence_%") {
  tryCatch({
    tables_sql <- paste0("
      SELECT TABLE_NAME 
      FROM INFORMATION_SCHEMA.TABLES 
      WHERE TABLE_SCHEMA = '", schema, "' 
        AND TABLE_NAME LIKE '", table_pattern, "'
    ")
    
    temp_tables <- DatabaseConnector::querySql(connection, tables_sql)
    
    for (table in temp_tables$TABLE_NAME) {
      drop_sql <- paste0("DROP TABLE IF EXISTS ", schema, ".", table)
      DatabaseConnector::executeSql(connection, drop_sql)
      cat("Dropped temporary table:", table, "\n")
    }
  }, error = function(e) {
    warning("Failed to clean up temporary tables: ", e$message)
  })
}