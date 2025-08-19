# computeGaps: Database-Sufficient Prevalence Analysis

A comprehensive R package for analyzing prevalence of diagnostic tests, procedures, and treatments in OMOP CDM databases using direct database queries with full support for temporal analysis, multiple cohorts, and advanced statistical reporting.

## Overview

The `computeGaps` package provides a complete solution for prevalence analysis in healthcare data research by:

1. **Direct Database Integration** - Connect to OMOP CDM databases without intermediate data files
2. **Flexible Cohort Analysis** - Support for single or multiple cohort prevalence comparisons
3. **Temporal Analysis** - Configurable time windows around index dates for longitudinal studies
4. **Multi-Domain Support** - Comprehensive coverage of all OMOP CDM clinical domains
5. **Statistical Reporting** - Advanced statistical measures including confidence intervals and comparative analysis
6. **Automated Quality Checks** - Built-in data validation and quality assessment tools

## Key Features

- **Database-Sufficient Analysis**: All computations performed directly in database without data extraction
- **Multi-Domain Coverage**: Supports all OMOP CDM domains (Condition, Procedure, Measurement, Drug, etc.)
- **Temporal Flexibility**: Configurable time windows for before/after index date analysis
- **Statistical Robustness**: Confidence intervals, comparative statistics, and hypothesis testing
- **Performance Optimized**: Efficient SQL generation with support for large datasets
- **Quality Assurance**: Built-in data validation and quality metrics

## Input Data Formats

| Parameter | Type | Description | Example | Required |
|-----------|------|-------------|---------|----------|
| `connection` | DBI connection | Database connection to OMOP CDM | `dbConnect(...)` | Yes |
| `cohort_table` | String | Full table name of cohort table | `"results.cohort"` | Yes |
| `cohort_id` | Integer/Vector | Cohort definition ID(s) | `1001` or `c(1001,1002)` | Yes |
| `concept_ids` | Integer vector | OMOP concept IDs to analyze | `c(3013721, 3004249)` | Yes* |
| `concept_sets` | Named list | Groups of related concepts | `list("Labs" = c(3013721, 3004249))` | Yes* |
| `time_window` | List | Days before/after index date | `list(start = -30, end = 30)` | No |
| `domains` | Character vector | OMOP domains to search | `c("Measurement", "Procedure")` | No |
| `include_descendants` | Logical | Include concept descendants | `TRUE` | No |
| `stratify_by` | Character vector | Stratification variables | `c("gender_concept_id", "age_group")` | No |

*Either `concept_ids` or `concept_sets` must be provided

## Output Data Formats

### Primary Results Table

| Column | Type | Description | Example |
|--------|------|-------------|---------|
| `analysis_id` | Character | Unique analysis identifier | `"PREV_001_20240101"` |
| `cohort_id` | Integer | Cohort definition ID | `1001` |
| `cohort_name` | Character | Human-readable cohort name | `"Type 2 Diabetes"` |
| `concept_id` | Integer | OMOP concept ID | `3013721` |
| `concept_name` | Character | Concept name from vocabulary | `"Hemoglobin A1c"` |
| `domain_id` | Character | OMOP domain | `"Measurement"` |
| `time_window` | Character | Time period analyzed | `"-30 to 30 days"` |
| `n_patients_total` | Integer | Total patients in cohort | `5000` |
| `n_patients_with_concept` | Integer | Patients with concept | `3250` |
| `prevalence_percent` | Numeric | Prevalence percentage | `65.0` |
| `prevalence_95ci_lower` | Numeric | Lower confidence interval | `63.2` |
| `prevalence_95ci_upper` | Numeric | Upper confidence interval | `66.8` |
| `first_occurrence_days_median` | Numeric | Median days to first occurrence | `15` |
| `total_occurrences` | Integer | Total concept occurrences | `4100` |

### Metadata Object

```r
metadata <- list(
  analysis_timestamp = "2024-01-01 10:30:00 UTC",
  package_version = "1.0.0",
  database_name = "Medicare Claims Database",
  cdm_version = "5.4",
  analysis_parameters = list(...),
  quality_metrics = data.frame(...)
)
```

## Installation

### From GitHub (Development Version)

```r
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install computeGaps from GitHub
devtools::install_github("A1exanderAlexeyuk/computeGaps")
```

### From CRAN (Stable Release)

```r
install.packages("computeGaps")
```

### System Dependencies

```r
# Required packages
required_packages <- c(
  "DBI",        # Database connectivity
  "dplyr",      # Data manipulation  
  "dbplyr",     # Database backend for dplyr
  "jsonlite",   # JSON handling
  "readr"       # File I/O
)

# Optional packages for enhanced functionality
optional_packages <- c(
  "RPostgreSQL", # PostgreSQL driver
  "odbc",        # ODBC connections
  "ggplot2",     # Advanced plotting
  "knitr",       # Report generation
  "openxlsx"     # Excel export
)

# Install all dependencies
install.packages(c(required_packages, optional_packages))
```

## Quick Start

### Basic Single Cohort Analysis

```r
library(computeGaps)
library(DBI)

# Establish database connection
con <- dbConnect(RPostgreSQL::PostgreSQL(),
                 host = "your-omop-server",
                 dbname = "omop_cdm", 
                 user = "username",
                 password = "password")

# Simple prevalence analysis
results <- analyze_prevalence(
  connection = con,
  cohort_table = "results.cohort",
  cohort_id = 1001,                        # Type 2 Diabetes cohort
  concept_ids = c(3013721, 3004249),       # HbA1c and Glucose tests
  time_window = list(start = -30, end = 30) # 60-day window around index
)

# View results
print(results$results)
print(results$metadata$quality_metrics)
```

### Multi-Cohort Comparison

```r
# Compare prevalence across cohorts
comparison_results <- compare_cohort_prevalence(
  connection = con,
  cohort_table = "results.cohort", 
  cohort_ids = c(1001, 1002, 1003),       # Diabetes, Prediabetes, Control
  concept_ids = c(3013721, 3004249),       # Lab tests
  time_window = list(start = -90, end = 90),
  statistical_tests = TRUE
)

# View statistical comparison
print(comparison_results$statistical_tests)
```

### Concept Set Analysis

```r
# Define related concept groups
diabetes_concept_sets <- list(
  "HbA1c_Tests" = c(3013721, 3004295, 3003309),
  "Glucose_Tests" = c(3004249, 3003458, 3007263),
  "Lipid_Panel" = c(3018586, 3020891, 3027114)
)

# Analyze concept sets
concept_results <- analyze_concept_sets(
  connection = con,
  cohort_table = "results.cohort",
  cohort_id = 1001,
  concept_sets = diabetes_concept_sets,
  time_window = list(start = -180, end = 180)
)

# View concept set prevalence
print(concept_results$concept_set_summary)
```

## Advanced Usage

### Temporal Trend Analysis

```r
# Analyze prevalence changes over time
temporal_results <- analyze_temporal_trends(
  connection = con,
  cohort_table = "results.cohort", 
  cohort_id = 1001,
  concept_ids = c(3013721),               # HbA1c testing
  time_periods = list(
    "Baseline" = list(start = -365, end = -91),
    "Pre-Index" = list(start = -90, end = -1),
    "Post-Index" = list(start = 1, end = 90),
    "Follow-up" = list(start = 91, end = 365)
  ),
  trend_analysis = TRUE
)

# Plot temporal trends
plot_temporal_trends(temporal_results, 
                    output_file = "hba1c_trends.png")
```

### Stratified Analysis

```r
# Prevalence by demographic groups
stratified_results <- analyze_prevalence_stratified(
  connection = con,
  cohort_table = "results.cohort",
  cohort_id = 1001,
  concept_ids = c(3013721, 3004249),
  stratify_by = c("gender_concept_id", "age_group"),
  age_groups = list(
    breaks = c(18, 35, 50, 65, 80, 100),
    labels = c("18-34", "35-49", "50-64", "65-79", "80+")
  ),
  include_statistical_tests = TRUE
)

# View stratified results
print(stratified_results$by_gender)
print(stratified_results$by_age_group)
```

### Batch Processing

```r
# Process multiple analyses efficiently
batch_config <- list(
  "Diabetes_Monitoring" = list(
    cohort_id = 1001,
    concept_sets = list(
      "Glucose_Tests" = c(3004249, 3003458),
      "HbA1c_Tests" = c(3013721, 3004295)
    ),
    time_window = list(start = -90, end = 90)
  ),
  
  "Lipid_Screening" = list(
    cohort_id = 1001, 
    concept_sets = list(
      "Cholesterol_Tests" = c(3027114, 3001308),
      "Triglyceride_Tests" = c(3022192, 3018586)
    ),
    time_window = list(start = -180, end = 180)
  )
)

# Execute batch analysis
batch_results <- execute_batch_analysis(
  connection = con,
  cohort_table = "results.cohort",
  batch_config = batch_config,
  parallel = TRUE,
  output_directory = "batch_outputs/"
)
```

## Supported OMOP Domains

| Domain | OMOP Table | Description | Key Fields | Example Concepts |
|--------|------------|-------------|------------|------------------|
| **Condition** | `condition_occurrence` | Diagnoses, medical conditions | `condition_concept_id`, `condition_start_date` | Type 2 Diabetes (201826), Hypertension (316866) |
| **Procedure** | `procedure_occurrence` | Medical procedures, surgeries | `procedure_concept_id`, `procedure_date` | Colonoscopy (4304593), Cardiac Catheterization (4006969) |
| **Measurement** | `measurement` | Lab tests, vital signs, clinical assessments | `measurement_concept_id`, `measurement_date`, `value_as_number` | HbA1c (3013721), Blood Pressure (3004249) |
| **Drug** | `drug_exposure` | Medications, prescriptions | `drug_concept_id`, `drug_exposure_start_date` | Metformin (1503297), Lisinopril (1308216) |
| **Device** | `device_exposure` | Medical devices, implants | `device_concept_id`, `device_exposure_start_date` | Pacemaker (4013057), Insulin Pump (4052536) |
| **Observation** | `observation` | Social history, surveys, assessments | `observation_concept_id`, `observation_date` | Smoking Status (4275495), BMI (3038553) |
| **Visit** | `visit_occurrence` | Healthcare encounters | `visit_concept_id`, `visit_start_date` | Inpatient Visit (9201), Outpatient Visit (9202) |
| **Note** | `note` | Clinical notes, reports | `note_type_concept_id`, `note_date` | Discharge Summary, Progress Note |

### Domain-Specific Configuration

```r
# Configure domain-specific parameters
domain_config <- list(
  Measurement = list(
    include_lab_values = TRUE,
    value_filters = list(min = 0, max = 1000),
    unit_standardization = TRUE
  ),
  
  Drug = list(
    minimum_days_supply = 1,
    include_drug_era = FALSE,
    route_restrictions = c(4132161, 4139906)  # Oral, Injection
  ),
  
  Procedure = list(
    exclude_administrative = TRUE,
    procedure_type_filter = c(38000267, 38000275)  # Primary, Secondary
  )
)

# Apply domain configuration
results <- analyze_prevalence(
  connection = con,
  cohort_table = "results.cohort",
  cohort_id = 1001,
  concept_ids = c(3013721, 1503297, 4304593),
  domain_config = domain_config
)
```

## Time Window Analysis

### Time Window Specification

Time windows define the period around each patient's cohort index date to search for concept occurrences:

```r
# Time window examples
time_windows <- list(
  # Single symmetric window
  "60_day_window" = list(start = -30, end = 30),
  
  # Asymmetric windows
  "baseline_period" = list(start = -365, end = -1),
  "followup_period" = list(start = 1, end = 365),
  
  # Complex multi-period analysis
  "comprehensive" = list(
    "pre_diagnosis" = list(start = -730, end = -181),
    "diagnosis_period" = list(start = -180, end = 180), 
    "post_diagnosis" = list(start = 181, end = 730)
  )
)
```

### Time Window Interpretation

- **Negative values**: Days before cohort index date
- **Positive values**: Days after cohort index date  
- **Day 0**: Cohort index date (included based on `include_index_date` parameter)
- **Inclusive bounds**: Both start and end dates are included in the analysis

### Multi-Period Analysis

```r
# Analyze prevalence across multiple time periods
multi_period_results <- analyze_multi_period_prevalence(
  connection = con,
  cohort_table = "results.cohort",
  cohort_id = 1001,
  concept_ids = c(3013721),
  time_periods = list(
    "Year_Before" = list(start = -365, end = -1),
    "Month_Before" = list(start = -30, end = -1), 
    "Index_Period" = list(start = -7, end = 7),
    "Month_After" = list(start = 1, end = 30),
    "Year_After" = list(start = 1, end = 365)
  ),
  compare_periods = TRUE
)
```

## Database Requirements

### Minimum OMOP CDM Requirements

| Requirement | Specification | Notes |
|-------------|---------------|--------|
| **CDM Version** | 5.3+ | Versions 5.3, 5.4, 6.0 supported |
| **Required Tables** | `person`, `observation_period`, `cohort` | Core tables for patient identification |
| **Domain Tables** | Based on analysis scope | `measurement`, `procedure_occurrence`, etc. |
| **Vocabulary Tables** | `concept`, `concept_relationship` | For concept hierarchy navigation |
| **Database Engine** | PostgreSQL, SQL Server, Oracle, BigQuery | Via DBI-compatible drivers |

### Database Connection Setup

#### PostgreSQL

```r
library(RPostgreSQL)

# PostgreSQL connection
con <- dbConnect(
  PostgreSQL(),
  host = "localhost",
  port = 5432,
  dbname = "omop_cdm",
  user = "omop_user", 
  password = "secure_password"
)
```

#### SQL Server

```r
library(odbc)

# SQL Server connection
con <- dbConnect(
  odbc::odbc(),
  driver = "ODBC Driver 17 for SQL Server",
  server = "sql-server-name",
  database = "OMOP_CDM",
  uid = "username",
  pwd = "password"
)
```

#### BigQuery

```r
library(bigrquery)

# BigQuery connection  
con <- dbConnect(
  bigrquery::bigquery(),
  project = "your-project-id",
  dataset = "omop_cdm_dataset",
  billing = "your-billing-project"
)
```

### Database Validation

```r
# Validate database structure and connectivity
validation_results <- validate_database_setup(
  connection = con,
  cdm_schema = "cdm",
  vocabulary_schema = "vocabulary", 
  results_schema = "results",
  required_cdm_version = "5.3",
  check_vocabulary_version = TRUE,
  validate_cohort_table = TRUE
)

if (!validation_results$valid) {
  print(validation_results$issues)
  print(validation_results$recommendations)
}
```

## Function Reference

### Core Analysis Functions

#### `analyze_prevalence()`

Main function for single cohort prevalence analysis.

```r
analyze_prevalence(
  connection,
  cohort_table, 
  cohort_id,
  concept_ids = NULL,
  concept_sets = NULL, 
  time_window = list(start = -30, end = 30),
  domains = c("Measurement", "Procedure", "Condition", "Drug"),
  include_descendants = TRUE,
  min_observation_days = 365,
  confidence_level = 0.95,
  suppress_small_counts = TRUE
)
```

#### `compare_cohort_prevalence()`

Compare prevalence between multiple cohorts with statistical testing.

```r
compare_cohort_prevalence(
  connection,
  cohort_table,
  cohort_ids,
  concept_ids,
  time_window = list(start = -30, end = 30),
  statistical_tests = c("chi_square", "fisher_exact"),
  effect_size_measures = c("odds_ratio", "risk_difference"),
  adjust_p_values = "fdr"
)
```

#### `analyze_concept_sets()`

Analyze prevalence for groups of related concepts.

```r
analyze_concept_sets(
  connection,
  cohort_table, 
  cohort_id,
  concept_sets,
  time_window = list(start = -30, end = 30),
  set_logic = "any",  # "any", "all", "majority"
  include_individual_concepts = TRUE
)
```

### Temporal Analysis Functions

#### `analyze_temporal_trends()`

Analyze prevalence trends over multiple time periods.

```r
analyze_temporal_trends(
  connection,
  cohort_table,
  cohort_id, 
  concept_ids,
  time_periods,
  trend_analysis = TRUE,
  seasonality_analysis = FALSE
)
```

#### `analyze_multi_period_prevalence()`

Compare prevalence across defined time periods.

```r
analyze_multi_period_prevalence(
  connection,
  cohort_table,
  cohort_id,
  concept_ids, 
  time_periods,
  compare_periods = TRUE,
  period_comparison_tests = c("mcnemar", "cochran_q")
)
```

### Stratification and Subgroup Analysis

#### `analyze_prevalence_stratified()`

Perform stratified prevalence analysis by demographic or clinical variables.

```r
analyze_prevalence_stratified(
  connection,
  cohort_table,
  cohort_id,
  concept_ids,
  stratify_by = c("gender_concept_id", "age_group"),
  age_groups = NULL,
  include_overall = TRUE,
  statistical_tests = TRUE
)
```

### Utility and Helper Functions

#### `validate_database_setup()`

Comprehensive database validation for OMOP CDM compatibility.

```r
validate_database_setup(
  connection,
  cdm_schema = "cdm", 
  vocabulary_schema = "vocabulary",
  results_schema = "results",
  cohort_table = "cohort"
)
```

#### `get_concept_hierarchy()`

Retrieve concept relationships and hierarchies.

```r
get_concept_hierarchy(
  connection,
  concept_ids,
  include_ancestors = TRUE,
  include_descendants = TRUE,
  relationship_types = c("Is a", "Subsumes")
)
```

#### `estimate_analysis_size()`

Estimate computational requirements for analysis.

```r
estimate_analysis_size(
  connection,
  cohort_table,
  cohort_ids, 
  concept_ids,
  time_windows
)
```

### Export and Reporting Functions

#### `export_results()`

Export analysis results in multiple formats.

```r
export_results(
  results,
  output_file = "prevalence_analysis",
  format = c("csv", "excel", "json"),
  include_metadata = TRUE
)
```

#### `generate_summary_report()`

Create comprehensive HTML analysis report.

```r
generate_summary_report(
  results,
  output_file = "analysis_report.html",
  template = "standard",
  include_plots = TRUE,
  custom_sections = NULL
)
```

## Testing and Examples

### Unit Tests

The package includes comprehensive unit tests covering all major functionality:

```r
# Run all package tests
devtools::test()

# Run specific test suites
testthat::test_file("tests/testthat/test-prevalence-analysis.R")
testthat::test_file("tests/testthat/test-database-functions.R") 
testthat::test_file("tests/testthat/test-temporal-analysis.R")
```

### Example Datasets

#### Synthetic Test Data

```r
# Load synthetic OMOP CDM test data
data("synpuf_cohort")        # Sample cohort definitions
data("synpuf_concepts")      # Sample concept mappings  
data("diabetes_concept_sets") # Diabetes-related concept sets

# Example analysis with test data
test_results <- analyze_prevalence(
  connection = test_db_connection(),
  cohort_table = "synpuf.cohort",
  cohort_id = synpuf_cohort$cohort_id[1],
  concept_ids = diabetes_concept_sets$glucose_tests
)
```

#### Validation Examples

```r
# Validate against known results
validation_suite <- run_validation_examples(
  connection = con,
  validation_data = "validation_scenarios.json"
)

# Check validation results
stopifnot(all(validation_suite$tests_passed))
```

### Performance Benchmarks

```r
# Benchmark analysis performance
benchmark_results <- benchmark_analysis_performance(
  connection = con,
  cohort_sizes = c(1000, 10000, 100000),
  concept_set_sizes = c(10, 100, 1000),
  time_window_sizes = c(30, 90, 365)
)

# View performance metrics
print(benchmark_results$timing_summary)
plot_benchmark_results(benchmark_results)
```

## Error Handling

### Comprehensive Error Management

The package provides robust error handling with informative messages and recovery suggestions:

```r
# Enable detailed error reporting
options(computeGaps.verbose_errors = TRUE)

# Example with error handling
tryCatch({
  results <- analyze_prevalence(
    connection = con,
    cohort_table = "invalid.table",  # Will trigger error
    cohort_id = 1001,
    concept_ids = c(3013721)
  )
}, error = function(e) {
  # Enhanced error information
  cat("Error Type:", class(e)[1], "\n")
  cat("Error Message:", e$message, "\n")
  
  # Automatic diagnostics
  if (inherits(e, "database_error")) {
    diagnostics <- diagnose_database_error(e, con)
    cat("Suggested Solutions:\n")
    print(diagnostics$suggestions)
  }
})
```

### Common Error Types and Solutions

#### Database Connection Errors

```r
# Test and diagnose connection issues
connection_diagnosis <- diagnose_connection_issues(
  connection_details = connectionDetails,
  test_queries = TRUE
)

if (!connection_diagnosis$connection_valid) {
  cat("Connection Issues Found:\n")
  print(connection_diagnosis$issues)
  cat("\nTroubleshooting Steps:\n") 
  print(connection_diagnosis$troubleshooting_steps)
}
```

#### Data Validation Errors

```r
# Validate input parameters before analysis
validation_result <- validate_analysis_inputs(
  cohort_table = "results.cohort",
  cohort_id = 1001,
  concept_ids = c(3013721, 3004249),
  time_window = list(start = -30, end = 30)
)

if (!validation_result$valid) {
  stop("Input validation failed: ", 
       paste(validation_result$errors, collapse = "; "))
}
```

#### Performance and Memory Errors

```r
# Monitor and manage memory usage
memory_manager <- initialize_memory_management(
  max_memory_gb = 8,
  warning_threshold = 0.8,
  cleanup_interval = 100
)

results <- analyze_prevalence_with_monitoring(
  connection = con,
  cohort_table = "results.cohort", 
  cohort_id = 1001,
  concept_ids = large_concept_list,
  memory_manager = memory_manager
)
```

### Logging and Debugging

```r
# Enable comprehensive logging
configure_logging(
  log_level = "INFO",
  log_file = "computeGaps_analysis.log",
  include_sql_queries = TRUE,
  include_performance_metrics = TRUE
)

# Run analysis with detailed logging
results <- analyze_prevalence(
  connection = con,
  cohort_table = "results.cohort",
  cohort_id = 1001, 
  concept_ids = c(3013721),
  debug_mode = TRUE
)

# Review analysis log
view_analysis_log("computeGaps_analysis.log")
```

## Performance Considerations

### Database Query Optimization

The package generates optimized SQL queries for efficient database performance:

```r
# Configure performance optimization
performance_config <- configure_performance_optimization(
  use_query_hints = TRUE,
  enable_parallel_processing = TRUE,
  batch_size = 10000,
  connection_pooling = TRUE,
  query_timeout_seconds = 300
)

# Apply performance configuration
set_performance_config(performance_config)
```

### Large Dataset Handling

```r
# Optimize for large cohorts and concept sets
large_dataset_config <- list(
  chunked_processing = list(
    enabled = TRUE,
    chunk_size = 50000,
    parallel_chunks = 4
  ),
  memory_optimization = list(
    stream_results = TRUE,
    garbage_collection_frequency = 1000,
    temp_table_cleanup = TRUE
  ),
  query_optimization = list(
    use_indexes = TRUE,
    optimize_joins = TRUE,
    predicate_pushdown = TRUE
  )
)

# Execute optimized analysis
results <- analyze_prevalence_optimized(
  connection = con,
  cohort_table = "results.cohort",
  cohort_id = 1001,
  concept_ids = very_large_concept_list,
  optimization_config = large_dataset_config
)
```

### Performance Monitoring

```r
# Monitor query performance
performance_monitor <- start_performance_monitoring()

results <- analyze_prevalence(
  connection = con,
  cohort_table = "results.cohort", 
  cohort_id = 1001,
  concept_ids = c(3013721, 3004249)
)

performance_metrics <- stop_performance_monitoring(performance_monitor)

# Review performance results
print(performance_metrics$summary)
print(performance_metrics$sql_execution_times)
print(performance_metrics$memory_usage_profile)
```

### Scalability Guidelines

| Dataset Size | Recommended Configuration | Expected Performance |
|--------------|---------------------------|---------------------|
| Small (<10K patients) | Default settings | <1 minute |
| Medium (10K-100K patients) | Enable chunked processing | 1-5 minutes |
| Large (100K-1M patients) | Parallel processing + optimization | 5-30 minutes |
| Very Large (>1M patients) | Full optimization + monitoring | 30+ minutes |

## Contributing Guidelines

We welcome contributions to improve `computeGaps`! Please follow these guidelines:

### Development Environment Setup

```r
# Clone repository
git clone https://github.com/A1exanderAlexeyuk/computeGaps.git
cd computeGaps

# Install development dependencies
devtools::install_dev_deps()

# Set up pre-commit hooks
usethis::use_pre_commit()
```

### Code Standards

- **Style Guide**: Follow the [tidyverse style guide](https://style.tidyverse.org/)
- **Documentation**: All functions must have complete roxygen2 documentation
- **Testing**: New functions require comprehensive unit tests (>90% coverage)
- **Performance**: Include performance benchmarks for computationally intensive functions

### Contribution Process

1. **Fork** the repository on GitHub
2. **Create** a feature branch (`git checkout -b feature/new-analysis-type`)
3. **Write** code following our standards
4. **Add** comprehensive tests and documentation
5. **Run** full test suite (`devtools::check()`)
6. **Submit** a pull request with detailed description

### Types of Contributions

- **Bug Fixes**: Report and fix issues in existing functionality
- **New Features**: Add new analysis types or statistical methods
- **Performance Improvements**: Optimize database queries or algorithms
- **Documentation**: Improve examples, tutorials, and API documentation
- **Testing**: Expand test coverage and validation examples

### Code Review Process

All contributions undergo peer review focusing on:

- Code quality and adherence to standards
- Test coverage and validation
- Performance impact assessment  
- Documentation completeness
- Backward compatibility

## License and Support

### License

This project is licensed under the MIT License:

```
MIT License

Copyright (c) 2024 computeGaps Contributors

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

### Support Resources

#### Primary Support Channels

- **GitHub Issues**: [https://github.com/A1exanderAlexeyuk/computeGaps/issues](https://github.com/A1exanderAlexeyuk/computeGaps/issues)
  - Bug reports and feature requests
  - Technical support and troubleshooting
  - Performance issues and optimization

- **Documentation**: [https://a1exanderalexeyuk.github.io/computeGaps/](https://a1exanderalexeyuk.github.io/computeGaps/)
  - Complete API reference
  - Detailed tutorials and examples
  - Best practices and guidelines

#### Community Support

- **OHDSI Forums**: [https://forums.ohdsi.org/](https://forums.ohdsi.org/)
  - Community discussions
  - OMOP CDM related questions
  - Research collaboration opportunities

- **Stack Overflow**: Tag questions with `computegaps` and `omop-cdm`
  - Programming and implementation questions
  - Integration with other tools
  - Performance optimization

#### Professional Support

For enterprise deployments, custom development, or consulting services:

- **Email**: computeGaps.support@example.com
- **Consultation**: Available for complex implementations and custom analysis requirements

### Citation

If you use `computeGaps` in your research, please cite:

```bibtex
@software{computeGaps2024,
  title = {computeGaps: Database-Sufficient Prevalence Analysis for OMOP CDM},
  author = {Alexander Alexeyuk and Contributors},
  year = {2024},
  version = {1.0.0},
  url = {https://github.com/A1exanderAlexeyuk/computeGaps},
  note = {R package version 1.0.0}
}
```

### Acknowledgments

- **OHDSI Collaborative** for the OMOP Common Data Model
- **R Core Team** for the R statistical computing environment
- **Contributors** who have helped improve this package
- **Research Community** for feedback and validation

### Version Information

- **Current Version**: 1.0.0
- **Release Date**: 2024-01-01
- **Compatibility**: R ≥ 4.0.0, OMOP CDM ≥ 5.3
- **Changelog**: See [CHANGELOG.md](CHANGELOG.md) for detailed version history