#' Simple Prevalence Calculation from TSV Data
#'
#' This function provides a simplified interface for calculating prevalence
#' of tests/procedures from TSV data without storing results in a database.
#' It returns results as a tibble.
#'
#' @param tsv_data Data frame or path to TSV file containing the analysis specifications
#' @param mock_mode Logical. If TRUE, generates mock data instead of querying a database
#' @param n_patients_per_cohort Integer. Number of patients per cohort (used in mock mode)
#' @param prevalence_range Numeric vector of length 2. Range for random prevalence generation in mock mode
#'
#' @return A tibble with columns: cohortname, omop_object_domain, object_custom_name, 
#'         Workflow stage, patient_count, n_patients, n_patients_with_op
#'
#' @examples
#' \dontrun{
#' # Example 1: Using sample data in mock mode
#' sample_data <- data.frame(
#'   cohortname = "Reference IBD Ulcerative colitis_InsightsGateway",
#'   concept_id_1 = 81893,
#'   relationship_id = "Has diagnostic test",
#'   concept_id_2 = 606840,
#'   omop_object_domain = "Procedure",
#'   object_custom_name = "Computed tomography of abdomen and pelvis",
#'   object_custom_code = "DxTest52",
#'   predicate_metadata = '{"Workflow stage": "Confirmatory Diagnosis", "time_gap_in_days": 7, "days_around_index_1": -14, "days_around_index_2": 0}',
#'   stringsAsFactors = FALSE
#' )
#' 
#' results <- calculate_prevalence_simple(sample_data, mock_mode = TRUE)
#' print(results)
#' 
#' # Example 2: Using TSV file path
#' results <- calculate_prevalence_simple("path/to/your/data.tsv", mock_mode = TRUE)
#' }
#'
#' @export
calculate_prevalence_simple <- function(tsv_data, 
                                      mock_mode = TRUE,
                                      n_patients_per_cohort = 1000,
                                      prevalence_range = c(5, 50)) {
  
  # Load required packages
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required")
  }
  
  # Read TSV if path is provided
  if (is.character(tsv_data) && length(tsv_data) == 1) {
    if (!file.exists(tsv_data)) {
      stop("TSV file not found: ", tsv_data)
    }
    tsv_data <- readr::read_tsv(tsv_data, show_col_types = FALSE)
  }
  
  # Validate required columns
  required_cols <- c("cohortname", "concept_id_1", "concept_id_2", 
                     "omop_object_domain", "object_custom_name", "predicate_metadata")
  missing_cols <- setdiff(required_cols, names(tsv_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Parse predicate metadata
  tsv_data$parsed_metadata <- lapply(tsv_data$predicate_metadata, function(x) {
    tryCatch({
      jsonlite::fromJSON(x)
    }, error = function(e) {
      warning("Failed to parse JSON metadata: ", x)
      list()
    })
  })
  
  # Extract metadata fields
  tsv_data$workflow_stage <- sapply(tsv_data$parsed_metadata, function(x) {
    if ("Workflow stage" %in% names(x)) x[["Workflow stage"]] else "Unknown"
  })
  
  tsv_data$days_around_index_1 <- sapply(tsv_data$parsed_metadata, function(x) {
    if ("days_around_index_1" %in% names(x)) as.numeric(x[["days_around_index_1"]]) else -30
  })
  
  tsv_data$days_around_index_2 <- sapply(tsv_data$parsed_metadata, function(x) {
    if ("days_around_index_2" %in% names(x)) as.numeric(x[["days_around_index_2"]]) else 30
  })
  
  # Get unique analysis combinations
  unique_analyses <- tsv_data %>%
    dplyr::select(cohortname, omop_object_domain, object_custom_name, 
                  workflow_stage, concept_id_2, days_around_index_1, days_around_index_2) %>%
    dplyr::distinct()
  
  # Calculate prevalence for each unique combination
  results_list <- list()
  
  for (i in seq_len(nrow(unique_analyses))) {
    row <- unique_analyses[i, ]
    
    if (mock_mode) {
      # Generate mock data
      set.seed(as.integer(row$concept_id_2) + i)  # For reproducible results
      
      # Total patients in cohort
      n_patients <- n_patients_per_cohort
      
      # Generate random prevalence percentage
      prevalence_pct <- runif(1, min = prevalence_range[1], max = prevalence_range[2])
      
      # Calculate number of patients with the test/procedure
      n_patients_with_op <- round(n_patients * prevalence_pct / 100)
      
      # Recalculate exact percentage
      patient_count <- round((n_patients_with_op / n_patients) * 100, 2)
      
    } else {
      stop("Non-mock mode requires database connection. Please use the full package functions or set mock_mode = TRUE")
    }
    
    # Create result row
    result_row <- tibble::tibble(
      cohortname = row$cohortname,
      omop_object_domain = row$omop_object_domain,
      object_custom_name = row$object_custom_name,
      `Workflow stage` = row$workflow_stage,
      patient_count = patient_count,  # This is the prevalence percentage
      n_patients = n_patients,
      n_patients_with_op = n_patients_with_op
    )
    
    results_list[[i]] <- result_row
  }
  
  # Combine all results
  final_results <- dplyr::bind_rows(results_list)
  
  # Add summary message
  cat("Prevalence calculation completed\n")
  cat("Total analyses:", nrow(final_results), "\n")
  cat("Unique cohorts:", length(unique(final_results$cohortname)), "\n")
  cat("Unique domains:", length(unique(final_results$omop_object_domain)), "\n")
  cat("Unique workflow stages:", length(unique(final_results$`Workflow stage`)), "\n")
  
  return(final_results)
}

#' Create Example TSV Data for Testing
#'
#' @param n_rows Number of rows to generate
#' @return Data frame with example TSV data
#'
#' @export
create_example_tsv_data <- function(n_rows = 10) {
  
  # Sample cohorts
  cohorts <- c(
    "Reference IBD Ulcerative colitis_InsightsGateway",
    "Reference IBD Crohn's disease_InsightsGateway",
    "Reference Diabetes Type 2_InsightsGateway"
  )
  
  # Sample tests/procedures
  tests <- list(
    list(
      concept_id = 606840,
      domain = "Procedure",
      name = "Computed tomography of abdomen and pelvis",
      code = "DxTest52"
    ),
    list(
      concept_id = 1091171,
      domain = "Measurement",
      name = "Clostridioides difficile toxin assay",
      code = "DxTest4"
    ),
    list(
      concept_id = 4024659,
      domain = "Measurement",
      name = "C-reactive protein measurement",
      code = "DxTest8"
    ),
    list(
      concept_id = 4184637,
      domain = "Measurement",
      name = "Hemoglobin A1c measurement",
      code = "LabTest1"
    )
  )
  
  # Sample workflow stages
  workflow_stages <- c("Confirmatory Diagnosis", "Initial Assessment", "Monitoring", "Follow-up")
  
  # Sample time windows
  time_windows <- list(
    list(days1 = -14, days2 = 0),
    list(days1 = -30, days2 = 7),
    list(days1 = 0, days2 = 30),
    list(days1 = -90, days2 = 90)
  )
  
  # Generate random combinations
  sample_data <- data.frame()
  
  for (i in 1:n_rows) {
    cohort <- sample(cohorts, 1)
    test <- sample(tests, 1)[[1]]
    stage <- sample(workflow_stages, 1)
    window <- sample(time_windows, 1)[[1]]
    
    # Create predicate metadata JSON
    metadata <- jsonlite::toJSON(list(
      `Workflow stage` = stage,
      time_gap_in_days = sample(c(7, 14, 30), 1),
      days_around_index_1 = window$days1,
      days_around_index_2 = window$days2
    ), auto_unbox = TRUE)
    
    row_data <- data.frame(
      cohortname = cohort,
      concept_id_1 = sample(c(81893, 201606, 201820), 1),
      relationship_id = "Has diagnostic test",
      concept_id_2 = test$concept_id,
      omop_object_domain = test$domain,
      object_custom_name = test$name,
      object_custom_code = test$code,
      predicate_metadata = as.character(metadata),
      stringsAsFactors = FALSE
    )
    
    sample_data <- rbind(sample_data, row_data)
  }
  
  return(sample_data)
}

#' Run Example Prevalence Calculation
#'
#' This function demonstrates the prevalence calculation with example data
#'
#' @param output_file Optional. Path to save results as TSV
#' @return Tibble with prevalence results
#'
#' @examples
#' \dontrun{
#' # Run example and view results
#' results <- run_prevalence_example()
#' print(results)
#' 
#' # Run example and save to file
#' results <- run_prevalence_example("prevalence_results.tsv")
#' }
#'
#' @export
run_prevalence_example <- function(output_file = NULL) {
  
  cat("=== Prevalence Calculation Example ===\n\n")
  
  # Create example data
  cat("Creating example TSV data...\n")
  example_data <- create_example_tsv_data(n_rows = 15)
  
  # Show sample of input data
  cat("\nSample input data:\n")
  print(head(example_data, 3))
  
  # Calculate prevalence
  cat("\nCalculating prevalence...\n")
  results <- calculate_prevalence_simple(example_data, mock_mode = TRUE)
  
  # Show results
  cat("\nResults:\n")
  print(results)
  
  # Create summary
  cat("\n=== Summary Statistics ===\n")
  summary_stats <- results %>%
    dplyr::group_by(`Workflow stage`) %>%
    dplyr::summarise(
      n_tests = dplyr::n(),
      avg_prevalence = round(mean(patient_count), 2),
      min_prevalence = round(min(patient_count), 2),
      max_prevalence = round(max(patient_count), 2),
      .groups = "drop"
    )
  print(summary_stats)
  
  # Save to file if requested
  if (!is.null(output_file)) {
    readr::write_tsv(results, output_file)
    cat("\nResults saved to:", output_file, "\n")
  }
  
  return(results)
}

#' Format Prevalence Results for Display
#'
#' @param results Tibble with prevalence results
#' @param digits Number of decimal places for percentages
#' @return Formatted tibble
#'
#' @export
format_prevalence_results <- function(results, digits = 1) {
  results %>%
    dplyr::mutate(
      prevalence_display = paste0(round(patient_count, digits), "%"),
      patients_display = paste0(n_patients_with_op, " / ", n_patients)
    ) %>%
    dplyr::select(
      cohortname,
      omop_object_domain,
      object_custom_name,
      `Workflow stage`,
      prevalence_display,
      patients_display,
      patient_count,
      n_patients,
      n_patients_with_op
    )
}