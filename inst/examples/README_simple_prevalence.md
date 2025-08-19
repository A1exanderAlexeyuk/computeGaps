# Simple Prevalence Calculation Guide

This guide explains how to use the simple prevalence calculation function that outputs results as a tibble without requiring database storage.

## Overview

The `calculate_prevalence_simple()` function calculates the prevalence of diagnostic tests/procedures within specified time windows around a cohort index date. It reads TSV data with the structure you provided and outputs results as a tibble.

## Input Data Structure

Your input data should have these columns:

- `cohortname`: Cohort identifier
- `concept_id_1`: Cohort concept ID
- `relationship_id`: Relationship type (e.g., "Has diagnostic test")
- `concept_id_2`: Test/procedure concept ID
- `omop_object_domain`: OMOP domain (Procedure, Measurement, etc.)
- `object_custom_name`: Human-readable name of the test/procedure
- `object_custom_code`: Custom code for the test/procedure
- `predicate_metadata`: JSON string containing:
  - `"Workflow stage"`: Stage in the clinical workflow
  - `"time_gap_in_days"`: Time gap parameter
  - `"days_around_index_1"`: Start of time window (negative = before index)
  - `"days_around_index_2"`: End of time window (positive = after index)

## Output Structure

The function returns a tibble with these columns:

- `cohortname`: Cohort identifier
- `omop_object_domain`: OMOP domain
- `object_custom_name`: Test/procedure name
- `Workflow stage`: Workflow stage from metadata
- `patient_count`: **Prevalence percentage** (% of patients with the test)
- `n_patients`: Total patients in cohort
- `n_patients_with_op`: Patients who had the test/procedure in the time window

### Understanding the Output

- `patient_count` is the prevalence percentage (0-100)
- `n_patients` is the total cohort size
- `n_patients_with_op` is the count of patients with the test
- Formula: `patient_count = (n_patients_with_op / n_patients) * 100`

## Basic Usage

```r
library(computeGaps)
library(tibble)

# Create sample data
sample_data <- tibble::tribble(
  ~cohortname, ~concept_id_1, ~relationship_id, ~concept_id_2, ~omop_object_domain, ~object_custom_name, ~object_custom_code, ~predicate_metadata,
  "Reference IBD Ulcerative colitis_InsightsGateway", 81893, "Has diagnostic test", 606840, "Procedure", "Computed tomography of abdomen and pelvis", "DxTest52", '{"Workflow stage": "Confirmatory Diagnosis", "time_gap_in_days": 7, "days_around_index_1": -14, "days_around_index_2": 0}',
  "Reference IBD Ulcerative colitis_InsightsGateway", 81893, "Has diagnostic test", 1091171, "Measurement", "Clostridioides difficile toxin assay", "DxTest4", '{"Workflow stage": "Confirmatory Diagnosis", "time_gap_in_days": 7, "days_around_index_1": -14, "days_around_index_2": 0}'
)

# Calculate prevalence (mock mode generates example data)
results <- calculate_prevalence_simple(sample_data, mock_mode = TRUE)

# View results
print(results)
```

## Working with TSV Files

```r
# Read from TSV file
results <- calculate_prevalence_simple("path/to/your/data.tsv", mock_mode = TRUE)

# Save results to TSV
readr::write_tsv(results, "prevalence_results.tsv")
```

## Formatting Results

```r
# Format results for better display
formatted_results <- format_prevalence_results(results)
print(formatted_results)

# This adds:
# - prevalence_display: "25.5%" format
# - patients_display: "255 / 1000" format
```

## Customization Options

```r
# Adjust mock data parameters
results <- calculate_prevalence_simple(
  tsv_data = sample_data,
  mock_mode = TRUE,
  n_patients_per_cohort = 2000,  # Cohort size
  prevalence_range = c(10, 60)    # Prevalence % range
)
```

## Summarizing Results

```r
# Summary by workflow stage
summary_by_stage <- results %>%
  group_by(`Workflow stage`) %>%
  summarise(
    n_tests = n(),
    avg_prevalence = round(mean(patient_count), 2),
    min_prevalence = round(min(patient_count), 2),
    max_prevalence = round(max(patient_count), 2),
    .groups = "drop"
  )

# Summary by domain
summary_by_domain <- results %>%
  group_by(omop_object_domain) %>%
  summarise(
    n_tests = n(),
    avg_prevalence = round(mean(patient_count), 2),
    total_patients_tested = sum(n_patients_with_op),
    .groups = "drop"
  )
```

## Filtering Results

```r
# High prevalence tests (>30%)
high_prevalence <- results %>%
  filter(patient_count > 30) %>%
  arrange(desc(patient_count))

# Specific workflow stage
confirmatory <- results %>%
  filter(`Workflow stage` == "Confirmatory Diagnosis")

# Specific domain
procedures <- results %>%
  filter(omop_object_domain == "Procedure")
```

## Complete Example

```r
# Run the complete example
source(system.file("examples/simple_prevalence_example.R", package = "computeGaps"))

# Or run the built-in example
results <- run_prevalence_example()
```

## Mock Mode vs Real Data

Currently, the function operates in **mock mode** which generates random prevalence data for testing. This is useful for:

- Testing your data pipeline
- Understanding the output format
- Developing downstream analyses

To use with real data, you would need to:
1. Use the full package functions with database connection
2. Or modify the function to read from pre-calculated data

## Time Windows Explained

The time windows in `predicate_metadata` define when to look for tests:

- `"days_around_index_1": -14` means 14 days BEFORE the index date
- `"days_around_index_2": 0` means ON the index date
- This creates a window from 14 days before to the index date

Common patterns:
- Pre-diagnosis: `[-30, 0]`
- Post-diagnosis monitoring: `[0, 90]`
- Extended follow-up: `[-90, 180]`

## Troubleshooting

### Missing columns error
Ensure your TSV has all required columns with exact names (case-sensitive).

### JSON parsing errors
Check that `predicate_metadata` contains valid JSON. Use double quotes inside the JSON string.

### No results
Check that your data has valid values and the time windows make sense for your use case.

## Next Steps

1. Prepare your TSV data with the required structure
2. Run `calculate_prevalence_simple()` in mock mode to test
3. Analyze the output using dplyr/tidyverse functions
4. Export results as needed

For database-connected analysis with real patient data, see the main package documentation.