# computeGaps: TSV-Based Prevalence Analysis for OMOP CDM

A streamlined R package for calculating prevalence of diagnostic tests, procedures, and treatments in OMOP CDM databases using TSV specification files. The package reads cohort and test specifications from TSV files, queries the database for prevalence calculations, and returns results as tibbles without any database writes.

## Overview

The `computeGaps` package provides a simple, file-based approach to prevalence analysis:

1. **TSV Input Files** - Define cohorts and tests/procedures to analyze in simple TSV format
2. **Read-Only Database Access** - Connect to OMOP CDM databases via DatabaseConnector for queries only
3. **Prevalence Calculation** - Calculate percentage of patients with specified tests/procedures
4. **Tibble Output** - Return results as in-memory tibbles with no database modifications
5. **No Database Writes** - Purely read-only operations, perfect for restricted environments

## Key Features

- **TSV-Driven Analysis**: Simple tab-separated files define analysis specifications
- **Read-Only Operations**: No temporary tables, no database writes required
- **Prevalence Percentages**: Calculate what percentage of cohort patients have specific tests/procedures
- **Time Window Support**: Analyze occurrences within specified days before/after index date
- **Multi-Domain Coverage**: Supports all OMOP CDM domains (Measurement, Procedure, Drug, etc.)
- **Memory Efficient**: Results returned as tibbles without intermediate storage

## TSV Input Format

### Cohort Specification TSV

Define patient cohorts to analyze:

```
cohort_id	cohort_name	cohort_table	description
1001	Type 2 Diabetes	results.cohort	Patients with T2DM diagnosis
1002	Hypertension	results.cohort	Patients with hypertension
1003	Control Group	results.cohort	Patients without diabetes or hypertension
```

### Test Specification TSV

Define tests/procedures to calculate prevalence for:

```
test_id	test_name	concept_id	domain	start_day	end_day	include_descendants
HBA1C_90	HbA1c Test (90 days)	3004410	Measurement	-90	90	TRUE
GLUCOSE_30	Glucose Test (30 days)	3004501	Measurement	-30	30	TRUE
LIPID_180	Lipid Panel (6 months)	3019900	Measurement	-180	180	TRUE
BP_CHECK	Blood Pressure Check	3004249	Measurement	-30	30	FALSE
```

### Column Descriptions

**Cohort Specification Columns:**
- `cohort_id`: Integer identifier for the cohort
- `cohort_name`: Human-readable name for reporting
- `cohort_table`: Full table name containing cohort definitions
- `description`: Optional description of cohort

**Test Specification Columns:**
- `test_id`: Unique identifier for the test/procedure
- `test_name`: Human-readable name for reporting
- `concept_id`: OMOP concept ID to search for
- `domain`: OMOP domain (Measurement, Procedure, Drug, etc.)
- `start_day`: Days before index date (negative values)
- `end_day`: Days after index date (positive values)
- `include_descendants`: Include concept descendants (TRUE/FALSE)

## Output Format

Results are returned as a tibble with prevalence percentages:

| Column | Type | Description | Example |
|--------|------|-------------|---------|
| `cohort_id` | Integer | Cohort identifier | `1001` |
| `cohort_name` | Character | Cohort name | `"Type 2 Diabetes"` |
| `test_id` | Character | Test identifier | `"HBA1C_90"` |
| `test_name` | Character | Test name | `"HbA1c Test (90 days)"` |
| `concept_id` | Integer | OMOP concept ID | `3004410` |
| `domain` | Character | OMOP domain | `"Measurement"` |
| `time_window` | Character | Analysis window | `"-90 to 90 days"` |
| `n_patients_total` | Integer | Total patients in cohort | `5000` |
| `n_patients_with_test` | Integer | Patients with test | `3250` |
| `prevalence_percent` | Numeric | Prevalence percentage | `65.0` |
| `query_timestamp` | POSIXct | When analysis was run | `2024-01-01 10:30:00` |

## Installation

```r
# Install from GitHub
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

devtools::install_github("A1exanderAlexeyuk/computeGaps")

# Required dependencies
install.packages(c(
  "DatabaseConnector",  # For database connectivity
  "dplyr",             # For data manipulation
  "readr",             # For TSV file reading
  "tibble"             # For tibble creation
))
```

## Quick Start Example

```r
library(computeGaps)
library(DatabaseConnector)

# 1. Create TSV specification files
# cohorts.tsv
cohort_spec <- "cohort_id\tcohort_name\tcohort_table\tdescription
1001\tType 2 Diabetes\tresults.cohort\tPatients with T2DM diagnosis
1002\tHypertension\tresults.cohort\tPatients with hypertension"

writeLines(cohort_spec, "cohorts.tsv")

# tests.tsv
test_spec <- "test_id\ttest_name\tconcept_id\tdomain\tstart_day\tend_day\tinclude_descendants
HBA1C_90\tHbA1c Test (90 days)\t3004410\tMeasurement\t-90\t90\tTRUE
GLUCOSE_30\tGlucose Test (30 days)\t3004501\tMeasurement\t-30\t30\tTRUE
METFORMIN_180\tMetformin (6 months)\t1503297\tDrug\t-180\t180\tTRUE"

writeLines(test_spec, "tests.tsv")

# 2. Connect to database (read-only)
connectionDetails <- createConnectionDetails(
  dbms = "postgresql",
  server = "your-omop-server/database",
  user = "readonly_user",
  password = "password"
)

con <- DatabaseConnector::connect(connectionDetails)

# 3. Calculate prevalence from TSV files
results <- calculate_prevalence_from_tsv(
  connection = con,
  cohort_file = "cohorts.tsv",
  test_file = "tests.tsv",
  cdm_schema = "cdm",
  vocabulary_schema = "vocabulary"
)

# 4. View results (returned as tibble)
print(results)

# Example output:
# # A tibble: 6 × 10
#   cohort_id cohort_name     test_id    test_name           concept_id domain      time_window    n_patients_total n_patients_with_test prevalence_percent
#       <int> <chr>           <chr>      <chr>                    <int> <chr>       <chr>                     <int>                <int>              <dbl>
# 1      1001 Type 2 Diabetes HBA1C_90   HbA1c Test (90 d…      3004410 Measurement -90 to 90 days             5000                 3250               65.0
# 2      1001 Type 2 Diabetes GLUCOSE_30 Glucose Test (30…      3004501 Measurement -30 to 30 days             5000                 4100               82.0
# 3      1001 Type 2 Diabetes METFORM…   Metformin (6 mon…      1503297 Drug        -180 to 180 d…             5000                 3800               76.0
# 4      1002 Hypertension    HBA1C_90   HbA1c Test (90 d…      3004410 Measurement -90 to 90 days             3000                  900               30.0
# 5      1002 Hypertension    GLUCOSE_30 Glucose Test (30…      3004501 Measurement -30 to 30 days             3000                 1500               50.0
# 6      1002 Hypertension    METFORM…   Metformin (6 mon…      1503297 Drug        -180 to 180 d…             3000                  300               10.0

# 5. Export results
readr::write_tsv(results, "prevalence_results.tsv")
```

## Advanced Usage

### Multiple Cohort Analysis

```r
# Create comprehensive cohort specification
cat("cohort_id\tcohort_name\tcohort_table\tdescription
1001\tDiabetes Type 1\tresults.cohort\tType 1 diabetes patients
1002\tDiabetes Type 2\tresults.cohort\tType 2 diabetes patients  
1003\tPrediabetes\tresults.cohort\tPrediabetes patients
1004\tControl\tresults.cohort\tNo diabetes diagnosis", 
file = "diabetes_cohorts.tsv")

# Create detailed test specification
cat("test_id\ttest_name\tconcept_id\tdomain\tstart_day\tend_day\tinclude_descendants
HBA1C_BASE\tHbA1c Baseline\t3004410\tMeasurement\t-365\t-1\tTRUE
HBA1C_FOLLOW\tHbA1c Follow-up\t3004410\tMeasurement\t1\t365\tTRUE
GLUCOSE_FASTING\tFasting Glucose\t3004501\tMeasurement\t-90\t90\tTRUE
INSULIN\tInsulin Use\t1530014\tDrug\t-180\t180\tTRUE
METFORMIN\tMetformin Use\t1503297\tDrug\t-180\t180\tTRUE
EYE_EXAM\tDiabetic Eye Exam\t4079938\tProcedure\t-365\t365\tTRUE
FOOT_EXAM\tDiabetic Foot Exam\t4144723\tProcedure\t-365\t365\tTRUE",
file = "diabetes_tests.tsv")

# Calculate prevalence
diabetes_results <- calculate_prevalence_from_tsv(
  connection = con,
  cohort_file = "diabetes_cohorts.tsv",
  test_file = "diabetes_tests.tsv",
  cdm_schema = "cdm",
  vocabulary_schema = "vocabulary"
)

# Analyze results by cohort type
library(dplyr)
summary_by_cohort <- diabetes_results %>%
  group_by(cohort_name, test_name) %>%
  summarise(
    prevalence = first(prevalence_percent),
    n_patients = first(n_patients_with_test)
  ) %>%
  pivot_wider(
    names_from = test_name,
    values_from = prevalence
  )

print(summary_by_cohort)
```

### Domain-Specific Analysis

```r
# Create domain-specific test files
# lab_tests.tsv
cat("test_id\ttest_name\tconcept_id\tdomain\tstart_day\tend_day\tinclude_descendants
HBA1C\tHemoglobin A1c\t3004410\tMeasurement\t-90\t90\tTRUE
GLUCOSE\tGlucose\t3004501\tMeasurement\t-90\t90\tTRUE
CREATININE\tCreatinine\t3016723\tMeasurement\t-90\t90\tTRUE
CHOLESTEROL\tTotal Cholesterol\t3027114\tMeasurement\t-180\t180\tTRUE
LDL\tLDL Cholesterol\t3028437\tMeasurement\t-180\t180\tTRUE",
file = "lab_tests.tsv")

# procedures.tsv  
cat("test_id\ttest_name\tconcept_id\tdomain\tstart_day\tend_day\tinclude_descendants
COLONOSCOPY\tColonoscopy\t4304593\tProcedure\t-1825\t1825\tTRUE
MAMMOGRAM\tMammography\t4324693\tProcedure\t-730\t730\tTRUE
EKG\tElectrocardiogram\t4232425\tProcedure\t-365\t365\tTRUE",
file = "procedures.tsv")

# medications.tsv
cat("test_id\ttest_name\tconcept_id\tdomain\tstart_day\tend_day\tinclude_descendants
STATIN\tStatin Use\t1539403\tDrug\t-180\t180\tTRUE
ACE_INHIBITOR\tACE Inhibitor\t1308216\tDrug\t-180\t180\tTRUE
BETA_BLOCKER\tBeta Blocker\t1307046\tDrug\t-180\t180\tTRUE",
file = "medications.tsv")

# Run separate analyses
lab_results <- calculate_prevalence_from_tsv(con, "cohorts.tsv", "lab_tests.tsv", "cdm", "vocabulary")
proc_results <- calculate_prevalence_from_tsv(con, "cohorts.tsv", "procedures.tsv", "cdm", "vocabulary")
med_results <- calculate_prevalence_from_tsv(con, "cohorts.tsv", "medications.tsv", "cdm", "vocabulary")

# Combine results
all_results <- bind_rows(
  lab_results %>% mutate(category = "Laboratory"),
  proc_results %>% mutate(category = "Procedure"),
  med_results %>% mutate(category = "Medication")
)
```

### Batch Processing Multiple TSV Files

```r
# Process multiple TSV pairs
tsv_pairs <- list(
  list(cohort = "diabetes_cohorts.tsv", test = "diabetes_tests.tsv", name = "Diabetes"),
  list(cohort = "cancer_cohorts.tsv", test = "cancer_tests.tsv", name = "Cancer"),
  list(cohort = "cardiac_cohorts.tsv", test = "cardiac_tests.tsv", name = "Cardiac")
)

# Run batch analysis
batch_results <- lapply(tsv_pairs, function(pair) {
  results <- calculate_prevalence_from_tsv(
    connection = con,
    cohort_file = pair$cohort,
    test_file = pair$test,
    cdm_schema = "cdm",
    vocabulary_schema = "vocabulary"
  )
  results$analysis_name <- pair$name
  return(results)
})

# Combine all results
all_batch_results <- bind_rows(batch_results)

# Save combined results
write_tsv(all_batch_results, "all_prevalence_results.tsv")
```

## Function Reference

### Main Function

#### `calculate_prevalence_from_tsv()`

Calculate prevalence from TSV specification files.

```r
calculate_prevalence_from_tsv(
  connection,           # DatabaseConnector connection object
  cohort_file,         # Path to cohort specification TSV
  test_file,           # Path to test specification TSV  
  cdm_schema = "cdm",  # Schema containing OMOP CDM tables
  vocabulary_schema = "vocabulary"  # Schema containing vocabulary tables
)
```

**Parameters:**
- `connection`: Active DatabaseConnector connection (read-only access is sufficient)
- `cohort_file`: Path to TSV file with cohort specifications
- `test_file`: Path to TSV file with test/procedure specifications
- `cdm_schema`: Database schema containing OMOP CDM clinical tables
- `vocabulary_schema`: Database schema containing OMOP vocabulary tables

**Returns:**
- A tibble containing prevalence results for each cohort-test combination

### Helper Functions

#### `read_cohort_spec()`

Read and validate cohort specification TSV file.

```r
cohort_spec <- read_cohort_spec("cohorts.tsv")
```

#### `read_test_spec()`

Read and validate test specification TSV file.

```r
test_spec <- read_test_spec("tests.tsv")
```

#### `validate_tsv_specs()`

Validate TSV specifications before processing.

```r
validation <- validate_tsv_specs(
  cohort_spec = cohort_spec,
  test_spec = test_spec
)

if (!validation$valid) {
  print(validation$errors)
}
```

## Database Requirements

### Minimum Requirements

- **OMOP CDM Version**: 5.3 or higher
- **Required Access**: READ-ONLY access to:
  - Cohort table (as specified in TSV)
  - OMOP clinical domain tables (measurement, procedure_occurrence, drug_exposure, etc.)
  - Vocabulary tables (concept, concept_ancestor)
- **No Write Access Required**: All operations are read-only queries

### Supported Database Systems

- PostgreSQL
- Microsoft SQL Server
- Oracle
- Google BigQuery
- Amazon Redshift
- Snowflake

### Example Connection Strings

```r
# PostgreSQL (read-only user)
connectionDetails <- createConnectionDetails(
  dbms = "postgresql",
  server = "localhost:5432/omop_cdm",
  user = "readonly_user",
  password = "password"
)

# SQL Server (Windows Authentication)
connectionDetails <- createConnectionDetails(
  dbms = "sql server",
  server = "server_name\\instance",
  trustedConnection = TRUE
)

# BigQuery (Service Account)
connectionDetails <- createConnectionDetails(
  dbms = "bigquery",
  connectionString = "jdbc:bigquery://...",
  user = "",
  password = ""
)
```

## Performance Considerations

### TSV File Size Limits

- **Cohorts**: Up to 10,000 cohorts per file
- **Tests**: Up to 1,000 tests per file
- **File Size**: TSV files should be under 10MB for optimal performance

### Query Optimization

The package automatically optimizes queries by:
- Using EXISTS clauses instead of JOINs where possible
- Batching multiple tests into single queries when appropriate
- Leveraging database indexes on concept_id and person_id

### Memory Usage

- Results are returned as tibbles, so memory usage scales with:
  - Number of cohorts × Number of tests × Result columns
- For very large analyses, consider processing in batches

## Troubleshooting

### Common Issues

#### TSV Parsing Errors

```r
# Check TSV file format
test_content <- readLines("tests.tsv")
cat(test_content, sep = "\n")

# Validate TSV structure
validation <- validate_tsv_structure("tests.tsv", type = "test")
print(validation$issues)
```

#### Database Connection Issues

```r
# Test connection
DatabaseConnector::dbIsValid(con)

# Test query access
test_query <- "SELECT COUNT(*) FROM cdm.person"
DatabaseConnector::querySql(con, test_query)
```

#### No Results Returned

```r
# Check cohort existence
cohort_check <- check_cohort_exists(
  connection = con,
  cohort_table = "results.cohort",
  cohort_id = 1001
)

# Check concept validity
concept_check <- validate_concepts(
  connection = con,
  concept_ids = c(3004410, 3004501),
  vocabulary_schema = "vocabulary"
)
```

## Citation

If you use `computeGaps` in your research, please cite:

```bibtex
@software{computeGaps2024,
  title = {computeGaps: TSV-Based Prevalence Analysis for OMOP CDM},
  author = {Alexander Alexeyuk and Contributors},
  year = {2024},
  version = {1.0.0},
  url = {https://github.com/A1exanderAlexeyuk/computeGaps},
  note = {R package for calculating prevalence from TSV specifications}
}
```

## License

This project is licensed under the MIT License. See LICENSE file for details.

## Support

- **GitHub Issues**: [https://github.com/A1exanderAlexeyuk/computeGaps/issues](https://github.com/A1exanderAlexeyuk/computeGaps/issues)
- **Documentation**: [https://a1exanderalexeyuk.github.io/computeGaps/](https://a1exanderalexeyuk.github.io/computeGaps/)