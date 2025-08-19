#' Compute prevalence of diagnostic tests/procedures around cohort index from a TSV specification
#'
#' This function reads a TSV that specifies pairs of concept_id_1 -> concept_id_2 relations
#' and associated metadata (including a time window around index) and computes, for each row,
#' how many cohort patients had the specified concept (concept_id_2) recorded in the appropriate
#' OMOP CDM domain table within the specified window relative to cohort_start_date.
#'
#' The TSV is expected to contain the following columns (case sensitive):
#' - cohortname: a label for the cohort (used in the output only)
#' - concept_id_1: not used in the computation, retained for completeness
#' - relationship_id: not used in the computation, retained for completeness
#' - concept_id_2: OMOP standard concept id for the diagnostic test/procedure/etc
#' - omop_object_domain: OMOP domain name for concept_id_2 (e.g., Measurement, Procedure, Observation, Drug, Condition, Device)
#' - object_custom_name: human-readable label for the concept/object
#' - object_custom_code: optional code, not used in the computation
#' - predicate_metadata: JSON string with keys including "Workflow stage", "days_around_index_1", "days_around_index_2", and optionally "time_gap_in_days".
#'
#' The output contains one row per input row with the following columns:
#' - cohortname
#' - omop_object_domain
#' - object_custom_name
#' - Workflow stage (from predicate_metadata)
#' - patient_count (distinct patients with at least one qualifying record)
#' - n_patients (distinct patients in the cohort table)
#' - n_patients_with_op (alias of patient_count for clarity)
#'
#' Notes/assumptions:
#' - The cohort table should contain only the cohort of interest OR you can supply a specific
#'   cohort_definition_id to filter within a shared cohort table.
#' - The time window is applied relative to cohort_start_date inclusive: [days_around_index_1, days_around_index_2].
#' - Exact concept_id matching is used (no descendant expansion).
#'
#' @param conn A DatabaseConnector connection object (DBI connection) to the CDM database.
#' @param cdm_schema The schema name where CDM tables live (e.g., "omop_cdm").
#' @param cohort_schema The schema name where the cohort table lives.
#' @param cohort_table The cohort table name (standard columns: subject_id, cohort_start_date [, cohort_definition_id]).
#' @param tsv_path Path to the TSV file with specifications.
#' @param cohort_definition_id Optional integer; if provided, will filter the cohort table by this ID.
#'
#' @return A tibble with the columns described above.
#' @export
compute_prevalence_from_tsv <- function(conn,
                                        cdm_schema,
                                        cohort_schema,
                                        cohort_table,
                                        tsv_path,
                                        cohort_definition_id = NULL) {
  stopifnot(!missing(conn), !missing(cdm_schema), !missing(cohort_schema), !missing(cohort_table), !missing(tsv_path))

  # Dependencies
  if (!requireNamespace("readr", quietly = TRUE)) stop("Package 'readr' is required")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' is required")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' is required")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required")
  if (!requireNamespace("purrr", quietly = TRUE)) stop("Package 'purrr' is required")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Package 'stringr' is required")
  if (!requireNamespace("SqlRender", quietly = TRUE)) stop("Package 'SqlRender' is required")
  if (!requireNamespace("DatabaseConnector", quietly = TRUE)) stop("Package 'DatabaseConnector' is required")

  # 1) Read TSV
  spec_df <- readr::read_tsv(tsv_path, show_col_types = FALSE, progress = FALSE, col_types = readr::cols(.default = readr::col_character()))

  required_cols <- c(
    "cohortname", "concept_id_1", "relationship_id", "concept_id_2",
    "omop_object_domain", "object_custom_name", "object_custom_code", "predicate_metadata"
  )
  missing_cols <- setdiff(required_cols, names(spec_df))
  if (length(missing_cols) > 0) {
    stop("TSV is missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # 2) Parse required fields from predicate_metadata JSON
  clean_json <- function(x) {
    # Remove possible wrapping quotes and un-escape quotes
    x <- stringr::str_trim(x %||% "")
    x <- stringr::str_replace_all(x, "^\\\"|\\\"$", "")
    x
  }
  `%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

  parsed <- purrr::map(spec_df$predicate_metadata, function(js) {
    js <- clean_json(js)
    # If empty or invalid JSON, return defaults
    out <- list(`Workflow stage` = NA_character_, days_around_index_1 = NA_real_, days_around_index_2 = NA_real_, time_gap_in_days = NA_real_)
    if (nzchar(js)) {
      try({
        tmp <- jsonlite::fromJSON(js)
        # Normalize names (handle possible different keys)
        out$`Workflow stage` <- tmp[["Workflow stage"]] %||% tmp[["workflow_stage"]] %||% NA_character_
        out$days_around_index_1 <- as.numeric(tmp[["days_around_index_1"]] %||% tmp[["days1"]] %||% NA_real_)
        out$days_around_index_2 <- as.numeric(tmp[["days_around_index_2"]] %||% tmp[["days2"]] %||% NA_real_)
        out$time_gap_in_days <- as.numeric(tmp[["time_gap_in_days"]] %||% tmp[["gap"]] %||% NA_real_)
      }, silent = TRUE)
    }
    out
  })

  meta_df <- tibble::tibble(
    workflow_stage = purrr::map_chr(parsed, ~ .x[["Workflow stage"]] %||% NA_character_),
    days1 = purrr::map_dbl(parsed, ~ .x[["days_around_index_1"]] %||% NA_real_),
    days2 = purrr::map_dbl(parsed, ~ .x[["days_around_index_2"]] %||% NA_real_),
    time_gap_in_days = purrr::map_dbl(parsed, ~ .x[["time_gap_in_days"]] %||% NA_real_)
  )

  spec_df2 <- dplyr::bind_cols(spec_df, meta_df)

  # normalize numeric types
  suppressWarnings({
    spec_df2$concept_id_2 <- as.integer(spec_df2$concept_id_2)
    spec_df2$days1 <- as.integer(spec_df2$days1)
    spec_df2$days2 <- as.integer(spec_df2$days2)
  })

  # Distinct analysis rows to avoid duplicate queries
  analysis_rows <- spec_df2 |>
    dplyr::select(cohortname, omop_object_domain, object_custom_name, concept_id_2, workflow_stage, days1, days2) |>
    dplyr::distinct()

  # 3) Compute counts from CDM
  # Helper: domain mapping
  domain_map <- list(
    Measurement = list(table = "measurement", concept_field = "measurement_concept_id", date_field = "measurement_date"),
    Procedure = list(table = "procedure_occurrence", concept_field = "procedure_concept_id", date_field = "procedure_date"),
    Observation = list(table = "observation", concept_field = "observation_concept_id", date_field = "observation_date"),
    Drug = list(table = "drug_exposure", concept_field = "drug_concept_id", date_field = "drug_exposure_start_date"),
    Condition = list(table = "condition_occurrence", concept_field = "condition_concept_id", date_field = "condition_start_date"),
    Device = list(table = "device_exposure", concept_field = "device_concept_id", date_field = "device_exposure_start_date"),
    Specimen = list(table = "specimen", concept_field = "specimen_concept_id", date_field = "specimen_date"),
    Visit = list(table = "visit_occurrence", concept_field = "visit_concept_id", date_field = "visit_start_date")
  )

  # n_patients in cohort
  sql_n_pat <- (
    "SELECT COUNT(DISTINCT subject_id) AS n_patients\n     FROM @cohort_schema.@cohort_table\n     {@cohort_definition_id != -999999} ? {WHERE cohort_definition_id = @cohort_definition_id} : {}"
  )
  sql_n_pat <- SqlRender::render(sql_n_pat,
                                 cohort_schema = cohort_schema,
                                 cohort_table = cohort_table,
                                 cohort_definition_id = ifelse(is.null(cohort_definition_id), -999999, cohort_definition_id))
  sql_n_pat <- SqlRender::translate(sql_n_pat, targetDialect = attr(conn, "dbms") %||% DatabaseConnector::dbms(conn))
  n_patients <- DatabaseConnector::querySql(conn, sql_n_pat, snakeCaseToCamelCase = TRUE)$N_PATIENTS[1]
  n_patients <- as.integer(n_patients %||% 0L)

  results <- list()

  for (i in seq_len(nrow(analysis_rows))) {
    row <- analysis_rows[i, ]
    domain <- as.character(row$omop_object_domain)
    map <- domain_map[[domain]]
    if (is.null(map)) {
      warning(sprintf("Unsupported omop_object_domain '%s' in row %d; skipping", domain, i))
      next
    }

    days1 <- as.integer(row$days1 %||% 0L)
    days2 <- as.integer(row$days2 %||% 0L)
    # Normalize window to ensure days1 <= days2
    w_start <- min(days1, days2, na.rm = TRUE)
    w_end <- max(days1, days2, na.rm = TRUE)

    sql_tmpl <- (
      "WITH cohort AS (\n         SELECT subject_id, cohort_start_date\n         FROM @cohort_schema.@cohort_table\n         {@cohort_definition_id != -999999} ? {WHERE cohort_definition_id = @cohort_definition_id} : {}\n       )\n       SELECT COUNT(DISTINCT c.subject_id) AS patient_count\n       FROM cohort c\n       JOIN @cdm_schema.@domain_table d\n         ON d.@concept_field = @concept_id\n        AND d.@date_field BETWEEN DATEADD(day, @w_start, c.cohort_start_date) AND DATEADD(day, @w_end, c.cohort_start_date)"
    )

    sql_rendered <- SqlRender::render(
      sql_tmpl,
      cohort_schema = cohort_schema,
      cohort_table = cohort_table,
      cohort_definition_id = ifelse(is.null(cohort_definition_id), -999999, cohort_definition_id),
      cdm_schema = cdm_schema,
      domain_table = map$table,
      concept_field = map$concept_field,
      date_field = map$date_field,
      concept_id = as.integer(row$concept_id_2),
      w_start = as.integer(w_start),
      w_end = as.integer(w_end)
    )
    sql_exec <- SqlRender::translate(sql_rendered, targetDialect = attr(conn, "dbms") %||% DatabaseConnector::dbms(conn))

    cnt <- DatabaseConnector::querySql(conn, sql_exec, snakeCaseToCamelCase = TRUE)$PATIENT_COUNT[1]
    cnt <- as.integer(cnt %||% 0L)

    results[[length(results) + 1]] <- tibble::tibble(
      cohortname = as.character(row$cohortname),
      omop_object_domain = domain,
      object_custom_name = as.character(row$object_custom_name),
      `Workflow stage` = as.character(row$workflow_stage %||% NA_character_),
      patient_count = cnt,
      n_patients = n_patients,
      n_patients_with_op = cnt
    )
  }

  if (length(results) == 0) {
    return(tibble::tibble(
      cohortname = character(),
      omop_object_domain = character(),
      object_custom_name = character(),
      `Workflow stage` = character(),
      patient_count = integer(),
      n_patients = integer(),
      n_patients_with_op = integer()
    ))
  }

  dplyr::bind_rows(results)
}
