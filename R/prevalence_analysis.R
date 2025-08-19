#' Read and Analyze TSV Data for Treatment Gaps Prevalence
#'
#' This function reads TSV data containing cohort information and performs
#' prevalence analysis of diagnostic tests within specified time windows.
#'
#' @param tsv_file_path Character. Path to the TSV file containing the data
#' @param connection Database connection object for OMOP CDM database
#' @param cohort_table_name Character. Name of the cohort table in the database
#' @param observation_period_table Character. Name of the observation period table (default: "observation_period")
#' @param procedure_occurrence_table Character. Name of the procedure occurrence table (default: "procedure_occurrence")
#' @param measurement_table Character. Name of the measurement table (default: "measurement")
#' @param condition_occurrence_table Character. Name of the condition occurrence table (default: "condition_occurrence")
#' @param drug_exposure_table Character. Name of the drug exposure table (default: "drug_exposure")
#' @param device_exposure_table Character. Name of the device exposure table (default: "device_exposure")
#' @param observation_table Character. Name of the observation table (default: "observation")
#'
#' @return Data frame with columns: cohortname, omop_object_domain, object_custom_name, workflow_stage, patient_count, n_patients, n_patients_with_op
#'
#' @export
#' @import dplyr
#' @import readr
#' @import jsonlite
#' @import DatabaseConnector
#'
#' @examples
#' \dontrun{
#' # Example usage
#' con <- DatabaseConnector::connect(...)
#' results <- analyze_prevalence_from_tsv(
#'   tsv_file_path = "data/cohort_data.tsv",
#'   connection = con,
#'   cohort_table_name = "my_cohort"
#' )
#' }
analyze_prevalence_from_tsv <- function(tsv_file_path,
                                       connection,
                                       cohort_table_name,
                                       observation_period_table = "observation_period",
                                       procedure_occurrence_table = "procedure_occurrence",
                                       measurement_table = "measurement",
                                       condition_occurrence_table = "condition_occurrence",
                                       drug_exposure_table = "drug_exposure",
                                       device_exposure_table = "device_exposure",
                                       observation_table = "observation") {
  
  # Read TSV file
  cat("Reading TSV file:", tsv_file_path, "\n")
  tsv_data <- readr::read_tsv(tsv_file_path, show_col_types = FALSE)
  
  # Validate required columns
  required_cols <- c("cohortname", "concept_id_1", "relationship_id", "concept_id_2", 
                     "omop_object_domain", "object_custom_name", "object_custom_code", 
                     "predicate_metadata")
  
  missing_cols <- setdiff(required_cols, names(tsv_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Parse predicate_metadata JSON
  cat("Parsing predicate metadata...\n")
  tsv_data$parsed_metadata <- lapply(tsv_data$predicate_metadata, function(x) {
    tryCatch({
      jsonlite::fromJSON(x)
    }, error = function(e) {
      warning("Failed to parse JSON: ", x)
      return(list())
    })
  })
  
  # Extract metadata fields
  tsv_data$workflow_stage <- sapply(tsv_data$parsed_metadata, function(x) {
    ifelse(is.null(x$`Workflow stage`), NA, x$`Workflow stage`)
  })
  
  tsv_data$time_gap_in_days <- sapply(tsv_data$parsed_metadata, function(x) {
    ifelse(is.null(x$time_gap_in_days), NA, as.numeric(x$time_gap_in_days))
  })
  
  tsv_data$days_around_index_1 <- sapply(tsv_data$parsed_metadata, function(x) {
    ifelse(is.null(x$days_around_index_1), NA, as.numeric(x$days_around_index_1))
  })
  
  tsv_data$days_around_index_2 <- sapply(tsv_data$parsed_metadata, function(x) {
    ifelse(is.null(x$days_around_index_2), NA, as.numeric(x$days_around_index_2))
  })
  
  # Initialize results data frame
  results <- data.frame()
  
  # Process each unique combination
  unique_combinations <- tsv_data %>%
    dplyr::select(cohortname, omop_object_domain, object_custom_name, workflow_stage,
                  concept_id_2, days_around_index_1, days_around_index_2) %>%
    dplyr::distinct()
  
  cat("Processing", nrow(unique_combinations), "unique combinations...\n")
  
  for (i in 1:nrow(unique_combinations)) {
    combo <- unique_combinations[i, ]
    
    cat("Processing combination", i, "of", nrow(unique_combinations), "\n")
    
    # Get total patients in cohort
    n_patients <- get_cohort_patient_count(connection, cohort_table_name, combo$cohortname)
    
    # Get patients with the specific procedure/measurement in the time window
    n_patients_with_op <- get_patients_with_concept_in_timeframe(
      connection = connection,
      cohort_table_name = cohort_table_name,
      cohortname = combo$cohortname,
      concept_id = combo$concept_id_2,
      domain = combo$omop_object_domain,
      days_start = combo$days_around_index_1,
      days_end = combo$days_around_index_2,
      procedure_occurrence_table = procedure_occurrence_table,
      measurement_table = measurement_table,
      condition_occurrence_table = condition_occurrence_table,
      drug_exposure_table = drug_exposure_table,
      device_exposure_table = device_exposure_table,
      observation_table = observation_table
    )
    
    # Calculate patient_count (prevalence percentage)
    patient_count <- ifelse(n_patients > 0, round((n_patients_with_op / n_patients) * 100, 2), 0)
    
    # Add to results
    result_row <- data.frame(
      cohortname = combo$cohortname,
      omop_object_domain = combo$omop_object_domain,
      object_custom_name = combo$object_custom_name,
      workflow_stage = combo$workflow_stage,
      patient_count = patient_count,
      n_patients = n_patients,
      n_patients_with_op = n_patients_with_op,
      stringsAsFactors = FALSE
    )
    
    results <- rbind(results, result_row)
  }
  
  cat("Analysis completed. Processed", nrow(results), "combinations.\n")
  return(results)
}

#' Get Patient Count from Cohort Table
#'
#' @param connection Database connection object
#' @param cohort_table_name Character. Name of the cohort table
#' @param cohortname Character. Name of the specific cohort
#'
#' @return Integer. Number of patients in the cohort
get_cohort_patient_count <- function(connection, cohort_table_name, cohortname) {
  
  # Build query to count distinct patients in cohort
  query <- paste0("
    SELECT COUNT(DISTINCT subject_id) as patient_count
    FROM ", cohort_table_name, "
    WHERE cohort_definition_id = (
      SELECT cohort_definition_id 
      FROM ", cohort_table_name, "_definition 
      WHERE cohort_name = '", cohortname, "'
    )")
  
  tryCatch({
    result <- DatabaseConnector::querySql(connection, query)
    return(as.integer(result$PATIENT_COUNT[1]))
  }, error = function(e) {
    warning("Error getting cohort patient count: ", e$message)
    return(0)
  })
}

#' Get Patients with Specific Concept in Timeframe
#'
#' @param connection Database connection object
#' @param cohort_table_name Character. Name of the cohort table
#' @param cohortname Character. Name of the specific cohort
#' @param concept_id Integer. OMOP concept ID to search for
#' @param domain Character. OMOP domain (Procedure, Measurement, etc.)
#' @param days_start Integer. Start of time window relative to index date
#' @param days_end Integer. End of time window relative to index date
#' @param procedure_occurrence_table Character. Name of procedure occurrence table
#' @param measurement_table Character. Name of measurement table
#' @param condition_occurrence_table Character. Name of condition occurrence table
#' @param drug_exposure_table Character. Name of drug exposure table
#' @param device_exposure_table Character. Name of device exposure table
#' @param observation_table Character. Name of observation table
#'
#' @return Integer. Number of patients with the concept in the specified timeframe
get_patients_with_concept_in_timeframe <- function(connection,
                                                  cohort_table_name,
                                                  cohortname,
                                                  concept_id,
                                                  domain,
                                                  days_start,
                                                  days_end,
                                                  procedure_occurrence_table,
                                                  measurement_table,
                                                  condition_occurrence_table,
                                                  drug_exposure_table,
                                                  device_exposure_table,
                                                  observation_table) {
  
  # Determine the appropriate table based on domain
  domain_table_map <- list(
    "Procedure" = procedure_occurrence_table,
    "Measurement" = measurement_table,
    "Condition" = condition_occurrence_table,
    "Drug" = drug_exposure_table,
    "Device" = device_exposure_table,
    "Observation" = observation_table
  )
  
  target_table <- domain_table_map[[domain]]
  if (is.null(target_table)) {
    warning("Unknown domain: ", domain)
    return(0)
  }
  
  # Determine the appropriate date and concept columns based on domain
  if (domain == "Procedure") {
    date_col <- "procedure_date"
    concept_col <- "procedure_concept_id"
  } else if (domain == "Measurement") {
    date_col <- "measurement_date"
    concept_col <- "measurement_concept_id"
  } else if (domain == "Condition") {
    date_col <- "condition_start_date"
    concept_col <- "condition_concept_id"
  } else if (domain == "Drug") {
    date_col <- "drug_exposure_start_date"
    concept_col <- "drug_concept_id"
  } else if (domain == "Device") {
    date_col <- "device_exposure_start_date"
    concept_col <- "device_concept_id"
  } else if (domain == "Observation") {
    date_col <- "observation_date"
    concept_col <- "observation_concept_id"
  } else {
    warning("Unsupported domain for date/concept mapping: ", domain)
    return(0)
  }
  
  # Build query to count patients with concept in timeframe
  query <- paste0("
    SELECT COUNT(DISTINCT c.subject_id) as patient_count
    FROM ", cohort_table_name, " c
    INNER JOIN ", target_table, " t ON c.subject_id = t.person_id
    WHERE c.cohort_definition_id = (
      SELECT cohort_definition_id 
      FROM ", cohort_table_name, "_definition 
      WHERE cohort_name = '", cohortname, "'
    )
    AND t.", concept_col, " = ", concept_id, "
    AND t.", date_col, " >= DATEADD(day, ", days_start, ", c.cohort_start_date)
    AND t.", date_col, " <= DATEADD(day, ", days_end, ", c.cohort_start_date)")
  
  tryCatch({
    result <- DatabaseConnector::querySql(connection, query)
    return(as.integer(result$PATIENT_COUNT[1]))
  }, error = function(e) {
    warning("Error getting patients with concept in timeframe: ", e$message)
    return(0)
  })
}

#' Write Results to TSV File
#'
#' @param results Data frame with analysis results
#' @param output_path Character. Path for output TSV file
#'
#' @export
write_prevalence_results <- function(results, output_path) {
  readr::write_tsv(results, output_path)
  cat("Results written to:", output_path, "\n")
}