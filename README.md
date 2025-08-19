# computeGaps

Calculate prevalence of diagnostic tests and procedures from TSV specification files for OMOP CDM cohorts.

## Overview

This package reads TSV files containing cohort and test specifications, queries OMOP CDM tables to calculate prevalence within specified time windows, and returns results as a tibble without writing to the database. The package uses database-friendly batch processing to efficiently handle large datasets by grouping queries by domain rather than executing individual queries per row.

## Installation

```r
# Install from GitHub
devtools::install_github("your-username/computeGaps")
```

## Usage

### Basic Usage

```r
library(computeGaps)
library(DatabaseConnector)

# Create database connection
conn <- DatabaseConnector::connect(
  dbms = "postgresql",
  server = "localhost/cdm",
  user = "user",
  password = "password"
)

# Calculate prevalence from TSV file
# Note: The package efficiently processes all rows by grouping them by domain,
# resulting in one SQL query per domain instead of per row
results <- compute_prevalence_from_tsv(
  conn = conn,
  cdm_schema = "cdm",
  cohort_schema = "results",
  cohort_table = "cohort",
  tsv_path = "path/to/specification.tsv",
  cohort_definition_id = 12345  # optional
)

# Results is a tibble with columns:
# cohortname, omop_object_domain, object_custom_name, Workflow stage,
# patient_count, n_patients, n_patients_with_op

DatabaseConnector::disconnect(conn)
```

### TSV File Format

The TSV file must contain these columns:
- `cohortname`: Cohort identifier
- `concept_id_1`: Source concept ID (for reference)
- `relationship_id`: Relationship type (for reference)
- `concept_id_2`: Target concept ID to calculate prevalence for
- `omop_object_domain`: OMOP domain (Procedure, Measurement, etc.)
- `object_custom_name`: Human-readable name
- `object_custom_code`: Custom code (optional)
- `predicate_metadata`: JSON with time window specification

Example TSV:
```
cohortname	concept_id_1	relationship_id	concept_id_2	omop_object_domain	object_custom_name	object_custom_code	predicate_metadata
IBD UC	81893	Has diagnostic test	606840	Procedure	CT abdomen/pelvis	DxTest52	{"Workflow stage": "Confirmatory Diagnosis", "time_gap_in_days": 7, "days_around_index_1": -14, "days_around_index_2": 0}
```

### Create Sample TSV

```r
# Create a sample TSV file for testing
create_sample_tsv("sample_analysis.tsv", n_rows = 5)
```

### Validate TSV Structure

```r
# Read and validate TSV
tsv_data <- read_and_validate_tsv("sample_analysis.tsv")

# Check structure
is_valid <- validate_tsv_structure(tsv_data, verbose = TRUE)
```

### Handling Large TSV Files

The package is optimized to efficiently handle TSV files with thousands of rows:

```r
# Create a large TSV file for demonstration
create_sample_tsv("large_analysis.tsv", n_rows = 5000)

# Process the large file - this will execute only ~8 queries (one per domain)
# instead of 5000 individual queries
system.time({
  results <- compute_prevalence_from_tsv(
    conn = conn,
    cdm_schema = "cdm",
    cohort_schema = "results",
    cohort_table = "cohort",
    tsv_path = "large_analysis.tsv"
  )
})

# The package groups all rows by domain and executes batch queries
# For example, if your 5000 rows span 3 domains:
# - 2000 rows for Procedure domain → 1 SQL query
# - 1800 rows for Measurement domain → 1 SQL query  
# - 1200 rows for Drug domain → 1 SQL query
# Total: 3 queries instead of 5000!
```

## Performance Optimization

The computeGaps package is designed for optimal database performance when processing large specification files:

### Batch Processing by Domain

Instead of executing one SQL query per row in your TSV file, the package intelligently groups all rows by their OMOP domain and executes a single query per domain. This dramatically reduces database load and improves performance.

**Example Performance Comparison:**
- Traditional approach: 1,000 rows = 1,000 SQL queries
- computeGaps approach: 1,000 rows across 5 domains = 5 SQL queries

### How It Works

1. The package reads your entire TSV file into memory
2. Groups all rows by their `omop_object_domain` value
3. For each domain group, constructs a single SQL query that:
   - Joins the cohort table with the appropriate domain table
   - Filters for all concept IDs in that domain group
   - Applies time window constraints
   - Calculates counts for all concepts in one pass
4. Results are then matched back to the original TSV rows

This approach is particularly beneficial when:
- Processing specifications with hundreds or thousands of tests/procedures
- Working with large cohorts
- Operating over slow network connections to the database
- Dealing with database query limits or throttling

## Supported OMOP Domains

- Measurement
- Procedure
- Observation
- Drug
- Condition
- Device
- Specimen
- Visit

## Output Format

The function returns a tibble with:
- `cohortname`: From input TSV
- `omop_object_domain`: OMOP domain
- `object_custom_name`: Test/procedure name
- `Workflow stage`: From predicate_metadata
- `patient_count`: Count of patients with the test/procedure
- `n_patients`: Total patients in cohort
- `n_patients_with_op`: Same as patient_count (alias for compatibility)

## Additional Functions

### Export Results

```r
# Export results to file
export_results(results, output_prefix = "prevalence_results", formats = c("tsv", "csv"))
```

### Create Summary Report

```r
# Generate text report
create_prevalence_report(results, output_path = "prevalence_report.txt")
```

### Summary Statistics

```r
# Get summary statistics
summary_stats <- create_summary_stats(results)
```