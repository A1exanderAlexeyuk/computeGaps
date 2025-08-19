#' TSV Data Reading and Parsing Functions
#'
#' Functions for reading and parsing TSV files containing
#' prevalence analysis configuration data.

#' Read and Validate TSV File
#'
#' @param tsv_file_path Character. Path to TSV file
#'
#' @return Data frame with validated TSV data
#' @keywords internal
read_and_validate_tsv <- function(tsv_file_path) {
  # Read TSV file
  tsv_data <- readr::read_tsv(
    tsv_file_path,
    col_types = readr::cols(
      cohortname = readr::col_character(),
      concept_id_1 = readr::col_double(),
      relationship_id = readr::col_character(),
      concept_id_2 = readr::col_double(),
      omop_object_domain = readr::col_character(),
      object_custom_name = readr::col_character(),
      object_custom_code = readr::col_character(),
      predicate_metadata = readr::col_character()
    ),
    show_col_types = FALSE
  )
  
  # Validate required columns
  required_columns <- c(
    "cohortname", "concept_id_1", "concept_id_2", 
    "omop_object_domain", "object_custom_name", "predicate_metadata"
  )
  
  missing_columns <- setdiff(required_columns, names(tsv_data))
  if (length(missing_columns) > 0) {
    stop("Missing required columns in TSV file: ", paste(missing_columns, collapse = ", "))
  }
  
  # Validate data types and values
  if (any(is.na(tsv_data$concept_id_1) | is.na(tsv_data$concept_id_2))) {
    stop("concept_id_1 and concept_id_2 cannot contain NA values")
  }
  
  if (any(is.na(tsv_data$cohortname) | tsv_data$cohortname == "")) {
    stop("cohortname cannot be empty or NA")
  }
  
  if (any(is.na(tsv_data$omop_object_domain) | tsv_data$omop_object_domain == "")) {
    stop("omop_object_domain cannot be empty or NA")
  }
  
  # Validate supported domains
  supported_domains <- c("Procedure", "Measurement", "Condition", "Drug", "Device", "Observation")
  invalid_domains <- setdiff(unique(tsv_data$omop_object_domain), supported_domains)
  if (length(invalid_domains) > 0) {
    stop("Unsupported domains found: ", paste(invalid_domains, collapse = ", "), 
         "\nSupported domains: ", paste(supported_domains, collapse = ", "))
  }
  
  return(tsv_data)
}

#' Parse Analysis Metadata from TSV
#'
#' @param tsv_data Data frame with TSV data
#'
#' @return Data frame with parsed metadata
#' @keywords internal
parse_analysis_metadata <- function(tsv_data) {
  results_list <- list()
  
  for (i in seq_len(nrow(tsv_data))) {
    row <- tsv_data[i, ]
    
    # Parse JSON metadata
    metadata <- parse_predicate_metadata(row$predicate_metadata)
    
    # Create result row
    result_row <- data.frame(
      cohortname = row$cohortname,
      concept_id_1 = row$concept_id_1,
      concept_id_2 = row$concept_id_2,
      omop_object_domain = row$omop_object_domain,
      object_custom_name = row$object_custom_name,
      object_custom_code = ifelse(is.na(row$object_custom_code), "", row$object_custom_code),
      workflow_stage = metadata$workflow_stage,
      time_gap_in_days = metadata$time_gap_in_days,
      days_around_index_1 = metadata$days_around_index_1,
      days_around_index_2 = metadata$days_around_index_2,
      stringsAsFactors = FALSE
    )
    
    results_list[[i]] <- result_row
  }
  
  return(do.call(rbind, results_list))
}

#' Parse Predicate Metadata JSON
#'
#' @param metadata_json Character. JSON string with metadata
#'
#' @return List with parsed metadata fields
#' @keywords internal
parse_predicate_metadata <- function(metadata_json) {
  tryCatch({
    metadata <- jsonlite::fromJSON(metadata_json)
    
    # Set defaults for missing fields
    result <- list(
      workflow_stage = ifelse(is.null(metadata$`Workflow stage`), "Unknown", metadata$`Workflow stage`),
      time_gap_in_days = ifelse(is.null(metadata$time_gap_in_days), 0, as.numeric(metadata$time_gap_in_days)),
      days_around_index_1 = ifelse(is.null(metadata$days_around_index_1), -30, as.numeric(metadata$days_around_index_1)),
      days_around_index_2 = ifelse(is.null(metadata$days_around_index_2), 30, as.numeric(metadata$days_around_index_2))
    )
    
    # Validate numeric fields
    if (is.na(result$time_gap_in_days)) result$time_gap_in_days <- 0
    if (is.na(result$days_around_index_1)) result$days_around_index_1 <- -30
    if (is.na(result$days_around_index_2)) result$days_around_index_2 <- 30
    
    return(result)
    
  }, error = function(e) {
    warning("Failed to parse metadata JSON: ", metadata_json, "\nError: ", e$message)
    return(list(
      workflow_stage = "Unknown",
      time_gap_in_days = 0,
      days_around_index_1 = -30,
      days_around_index_2 = 30
    ))
  })
}

#' Get Unique Analysis Configurations
#'
#' @param analysis_params Data frame with analysis parameters
#'
#' @return Data frame with unique analysis configurations
#' @keywords internal
get_unique_analyses <- function(analysis_params) {
  unique_configs <- unique(analysis_params[, c("cohortname", "workflow_stage")])
  return(unique_configs)
}

#' Validate TSV Structure
#'
#' @param tsv_data Data frame with TSV data
#' @param verbose Logical. Whether to print validation messages
#'
#' @return Logical indicating if validation passed
#' @export
validate_tsv_structure <- function(tsv_data, verbose = TRUE) {
  errors <- character(0)
  warnings <- character(0)
  
  # Check required columns
  required_columns <- c(
    "cohortname", "concept_id_1", "concept_id_2", 
    "omop_object_domain", "object_custom_name", "predicate_metadata"
  )
  
  missing_columns <- setdiff(required_columns, names(tsv_data))
  if (length(missing_columns) > 0) {
    errors <- c(errors, paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }
  
  if (length(errors) == 0) {
    # Check data quality
    if (any(is.na(tsv_data$concept_id_1) | is.na(tsv_data$concept_id_2))) {
      errors <- c(errors, "Concept IDs cannot contain NA values")
    }
    
    if (any(is.na(tsv_data$cohortname) | tsv_data$cohortname == "")) {
      errors <- c(errors, "Cohort names cannot be empty or NA")
    }
    
    # Check JSON metadata
    invalid_json_rows <- c()
    for (i in seq_len(nrow(tsv_data))) {
      tryCatch({
        jsonlite::fromJSON(tsv_data$predicate_metadata[i])
      }, error = function(e) {
        invalid_json_rows <<- c(invalid_json_rows, i)
      })
    }
    
    if (length(invalid_json_rows) > 0) {
      warnings <- c(warnings, paste("Invalid JSON metadata in rows:", paste(invalid_json_rows, collapse = ", ")))
    }
    
    # Check domains
    supported_domains <- c("Procedure", "Measurement", "Condition", "Drug", "Device", "Observation")
    invalid_domains <- setdiff(unique(tsv_data$omop_object_domain), supported_domains)
    if (length(invalid_domains) > 0) {
      errors <- c(errors, paste("Unsupported domains:", paste(invalid_domains, collapse = ", ")))
    }
  }
  
  # Print results
  if (verbose) {
    if (length(errors) == 0 && length(warnings) == 0) {
      message("✓ TSV structure validation passed successfully")
    } else {
      if (length(errors) > 0) {
        message("✗ Validation errors:")
        for (error in errors) {
          message("  - ", error)
        }
      }
      if (length(warnings) > 0) {
        message("⚠ Validation warnings:")
        for (warning in warnings) {
          message("  - ", warning)
        }
      }
    }
  }
  
  return(length(errors) == 0)
}

#' Create Sample TSV Data
#'
#' @param output_path Character. Path where to save sample TSV file
#' @param n_rows Integer. Number of sample rows to generate
#'
#' @return Character. Path to created sample file
#' @export
create_sample_tsv <- function(output_path = "sample_analysis_data.tsv", n_rows = 5) {
  sample_data <- data.frame(
    cohortname = rep(c("Reference IBD Ulcerative colitis_InsightsGateway", "Reference IBD Crohn disease_InsightsGateway"), length.out = n_rows),
    concept_id_1 = rep(c(81893, 81894), length.out = n_rows),
    relationship_id = rep("Has diagnostic test", n_rows),
    concept_id_2 = c(606840, 1091171, 4307814, 4124436, 4051466)[1:n_rows],
    omop_object_domain = rep(c("Procedure", "Measurement", "Procedure", "Measurement", "Condition"), length.out = n_rows),
    object_custom_name = c(
      "Computed tomography of abdomen and pelvis",
      "Clostridioides difficile toxin assay", 
      "Colonoscopy",
      "C-reactive protein measurement",
      "Inflammatory bowel disease"
    )[1:n_rows],
    object_custom_code = paste0("DxTest", 1:n_rows),
    predicate_metadata = rep(c(
      '{"Workflow stage": "Confirmatory Diagnosis", "time_gap_in_days": 7, "days_around_index_1": -14, "days_around_index_2": 0}',
      '{"Workflow stage": "Initial Assessment", "time_gap_in_days": 30, "days_around_index_1": -30, "days_around_index_2": 30}',
      '{"Workflow stage": "Follow-up", "time_gap_in_days": 90, "days_around_index_1": 0, "days_around_index_2": 90}'
    ), length.out = n_rows),
    stringsAsFactors = FALSE
  )
  
  readr::write_tsv(sample_data, output_path)
  message("Sample TSV file created: ", output_path)
  return(output_path)
}