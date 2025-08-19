# computeGaps

Calculate prevalence of diagnostic tests and procedures from TSV specification files for OMOP CDM cohorts.

## Overview

This package reads TSV files containing cohort and test specifications, queries OMOP CDM tables to calculate prevalence within specified time windows, and returns results as a tibble without writing to the database.

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