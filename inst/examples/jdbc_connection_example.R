# JDBC Connection Examples for computeGaps Package
# 
# This script demonstrates how to connect to various databases using
# DatabaseConnector with JDBC drivers (no ODBC required)

library(computeGaps)
library(DatabaseConnector)

# =============================================================================
# PostgreSQL Connection
# =============================================================================

# Download PostgreSQL JDBC driver if needed
# DatabaseConnector::downloadJdbcDrivers("postgresql")

postgres_connection <- DatabaseConnector::connect(
  dbms = "postgresql",
  server = "localhost/omop_cdm",  # server/database format
  user = "your_username",
  password = "your_password",
  port = 5432
)

# =============================================================================
# SQL Server Connection
# =============================================================================

# Download SQL Server JDBC driver if needed
# DatabaseConnector::downloadJdbcDrivers("sql server")

sqlserver_connection <- DatabaseConnector::connect(
  dbms = "sql server",
  server = "your_server_name",
  database = "your_database_name",
  user = "your_username",
  password = "your_password",
  port = 1433
)

# =============================================================================
# Oracle Connection
# =============================================================================

# Download Oracle JDBC driver if needed
# DatabaseConnector::downloadJdbcDrivers("oracle")

oracle_connection <- DatabaseConnector::connect(
  dbms = "oracle",
  server = "your_server_name/your_service_name",
  user = "your_username",
  password = "your_password",
  port = 1521
)

# =============================================================================
# BigQuery Connection
# =============================================================================

bigquery_connection <- DatabaseConnector::connect(
  dbms = "bigquery",
  connectionString = "jdbc:bigquery://https://www.googleapis.com/bigquery/v2:443;ProjectId=your_project_id;OAuthType=0;OAuthServiceAcctEmail=your_service_account@your_project.iam.gserviceaccount.com;OAuthPvtKeyPath=/path/to/key.json;",
  user = "",
  password = ""
)

# =============================================================================
# Redshift Connection
# =============================================================================

redshift_connection <- DatabaseConnector::connect(
  dbms = "redshift",
  server = "your_cluster.region.redshift.amazonaws.com/your_database",
  user = "your_username",
  password = "your_password",
  port = 5439
)

# =============================================================================
# Test Connection and Run Analysis
# =============================================================================

# Function to test any connection
test_connection <- function(connection, connection_name) {
  cat("\n=== Testing", connection_name, "Connection ===\n")
  
  tryCatch({
    # Test basic query
    result <- DatabaseConnector::querySql(connection, "SELECT 1 as test")
    cat("✓ Connection successful!\n")
    
    # Get database info
    tables <- DatabaseConnector::getTableNames(connection, "public")
    cat("✓ Found", length(tables), "tables in public schema\n")
    
    return(TRUE)
  }, error = function(e) {
    cat("✗ Connection failed:", e$message, "\n")
    return(FALSE)
  })
}

# Test connections (uncomment the one you're using)
# test_connection(postgres_connection, "PostgreSQL")
# test_connection(sqlserver_connection, "SQL Server")
# test_connection(oracle_connection, "Oracle")

# =============================================================================
# Run Prevalence Analysis
# =============================================================================

# Example analysis using any of the above connections
run_example_analysis <- function(connection) {
  # Create sample TSV
  sample_tsv <- "example_analysis.tsv"
  create_sample_tsv(sample_tsv, n_rows = 5)
  
  # Run analysis
  results_table <- analyze_prevalence_from_tsv_db(
    tsv_file_path = sample_tsv,
    connection = connection,
    cohort_database_schema = "results_schema",
    cohort_table_name = "cohort",
    cdm_database_schema = "cdm_schema",
    results_table_name = "prevalence_results",
    verbose = TRUE
  )
  
  # Query results
  results <- query_prevalence_results(
    connection = connection,
    cohort_database_schema = "results_schema",
    results_table_name = "prevalence_results"
  )
  
  print(head(results))
  
  # Clean up
  file.remove(sample_tsv)
  
  return(results)
}

# =============================================================================
# Connection Management Best Practices
# =============================================================================

# Always disconnect when done
# DatabaseConnector::disconnect(postgres_connection)
# DatabaseConnector::disconnect(sqlserver_connection)
# DatabaseConnector::disconnect(oracle_connection)

cat("\n=== JDBC Connection Examples Complete ===\n")
cat("Remember to:\n")
cat("1. Download JDBC drivers using DatabaseConnector::downloadJdbcDrivers()\n")
cat("2. Update connection parameters with your actual values\n")
cat("3. Always disconnect when finished\n")
cat("4. Check Java version compatibility (Java 8+ required)\n")