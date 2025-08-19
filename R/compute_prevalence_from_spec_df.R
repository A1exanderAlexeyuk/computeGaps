#' Compute prevalence from an in-memory spec tibble (tibble-only output, no DB writes)
#'
#' This function accepts a preloaded spec data.frame/tibble with the columns:
#' cohortname, concept_id_1, relationship_id, concept_id_2, omop_object_domain,
#' object_custom_name, object_custom_code, predicate_metadata (JSON), and computes
#' the prevalence of concept_id_2 events in the appropriate OMOP domain tables
#' within the specified time window around cohort_start_date. It performs only
#' read queries against the database and returns a tibble; it does not write to DB.
#'
#' The output columns match:
#' - cohortname
#' - omop_object_domain
#' - object_custom_name
#' - Workflow stage (from predicate_metadata)
#' - patient_count (distinct patients with at least one qualifying record)
#' - n_patients (distinct patients in the cohort table after optional filtering)
#' - n_patients_with_op (alias of patient_count)
#'
#' @param conn DatabaseConnector connection (OHDSI DatabaseConnector)
#' @param cdm_schema CDM schema name (e.g., "omop_cdm")
#' @param cohort_schema Schema that contains the cohort table
#' @param cohort_table Cohort table name (columns: subject_id, cohort_start_date [, cohort_definition_id])
#' @param spec_df Data frame/tibble with columns:
#'   cohortname, concept_id_1, relationship_id, concept_id_2,
#'   omop_object_domain, object_custom_name, object_custom_code, predicate_metadata (JSON)
#' @param cohort_definition_id Optional integer. If provided, filters cohort table by this ID
#' @param restrict_to_observation_period Logical. If TRUE, enforces that domain event dates fall within observation_period
#'
#' @return tibble with columns:
#'   cohortname, omop_object_domain, object_custom_name, `Workflow stage`,
#'   patient_count, n_patients, n_patients_with_op
#' @export
compute_prevalence_from_spec_df <- function(conn,
                                            cdm_schema,
                                            cohort_schema,
                                            cohort_table,
                                            spec_df,
                                            cohort_definition_id = NULL,
                                            restrict_to_observation_period = FALSE) {
  stopifnot(!missing(conn), !missing(cdm_schema), !missing(cohort_schema), !missing(cohort_table), !missing(spec_df))

  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' is required")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required")
  if (!requireNamespace("purrr", quietly = TRUE)) stop("Package 'purrr' is required")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Package 'stringr' is required")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' is required")
  if (!requireNamespace("SqlRender", quietly = TRUE)) stop("Package 'SqlRender' is required")
  if (!requireNamespace("DatabaseConnector", quietly = TRUE)) stop("Package 'DatabaseConnector' is required")

  `%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

  required_cols <- c(
    "cohortname", "concept_id_1", "relationship_id", "concept_id_2",
    "omop_object_domain", "object_custom_name", "object_custom_code", "predicate_metadata"
  )
  missing_cols <- setdiff(required_cols, names(spec_df))
  if (length(missing_cols) > 0) {
    stop("spec_df is missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Parse predicate_metadata JSON safely
  clean_json <- function(x) {
    x <- stringr::str_trim(x %||% "")
    x <- stringr::str_replace_all(x, "^\\\"|\\\"$", "")
    x
  }
  parsed <- purrr::map(spec_df$predicate_metadata, function(js) {
    js <- clean_json(js)
    out <- list(`Workflow stage` = NA_character_, days_around_index_1 = NA_real_, days_around_index_2 = NA_real_, time_gap_in_days = NA_real_)
    if (nzchar(js)) {
      try({
        tmp <- jsonlite::fromJSON(js)
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
    days1 = suppressWarnings(as.integer(purrr::map_dbl(parsed, ~ .x[["days_around_index_1"]] %||% NA_real_))),
    days2 = suppressWarnings(as.integer(purrr::map_dbl(parsed, ~ .x[["days_around_index_2"]] %||% NA_real_))),
    time_gap_in_days = suppressWarnings(as.integer(purrr::map_dbl(parsed, ~ .x[["time_gap_in_days"]] %||% NA_real_)))
  )

  spec_df2 <- dplyr::bind_cols(spec_df, meta_df)
  suppressWarnings(spec_df2$concept_id_2 <- as.integer(spec_df2$concept_id_2))

  # Domain mapping
  domain_map <- list(
    Measurement = list(table = "measurement", concept_field = "measurement_concept_id", date_field = "measurement_date"),
    Procedure   = list(table = "procedure_occurrence", concept_field = "procedure_concept_id", date_field = "procedure_date"),
    Observation = list(table = "observation", concept_field = "observation_concept_id", date_field = "observation_date"),
    Drug        = list(table = "drug_exposure", concept_field = "drug_concept_id", date_field = "drug_exposure_start_date"),
    Condition   = list(table = "condition_occurrence", concept_field = "condition_concept_id", date_field = "condition_start_date"),
    Device      = list(table = "device_exposure", concept_field = "device_concept_id", date_field = "device_exposure_start_date"),
    Specimen    = list(table = "specimen", concept_field = "specimen_concept_id", date_field = "specimen_date"),
    Visit       = list(table = "visit_occurrence", concept_field = "visit_concept_id", date_field = "visit_start_date")
  )

  # n_patients in cohort (optionally filter by cohort_definition_id)
  sql_n_pat <- "\n    SELECT COUNT(DISTINCT subject_id) AS n_patients\n    FROM @cohort_schema.@cohort_table\n    {@cohort_definition_id != -999999} ? {WHERE cohort_definition_id = @cohort_definition_id} : {}\n  "
  sql_n_pat <- SqlRender::render(
    sql_n_pat,
    cohort_schema = cohort_schema,
    cohort_table = cohort_table,
    cohort_definition_id = ifelse(is.null(cohort_definition_id), -999999, cohort_definition_id)
  )
  sql_n_pat <- SqlRender::translate(sql_n_pat, targetDialect = attr(conn, "dbms") %||% DatabaseConnector::dbms(conn))
  n_patients <- DatabaseConnector::querySql(conn, sql_n_pat, snakeCaseToCamelCase = TRUE)$N_PATIENTS[1]
  n_patients <- as.integer(n_patients %||% 0L)

  # Prepare unique analysis rows
  analysis_rows <- spec_df2 |>
    dplyr::select(cohortname, omop_object_domain, object_custom_name, concept_id_2, workflow_stage, days1, days2) |>
    dplyr::distinct()

  results <- list()

  for (i in seq_len(nrow(analysis_rows))) {
    row <- analysis_rows[i, ]
    domain <- as.character(row$omop_object_domain)
    map <- domain_map[[domain]]
    if (is.null(map)) {
      warning(sprintf("Unsupported omop_object_domain '%s' in row %d; skipping", domain, i))
      next
    }

    w_start <- min(as.integer(row$days1 %||% 0L), as.integer(row$days2 %||% 0L), na.rm = TRUE)
    w_end   <- max(as.integer(row$days1 %||% 0L), as.integer(row$days2 %||% 0L), na.rm = TRUE)

    # Base cohort CTE
    cohort_cte <- "\n      WITH cohort AS (\n        SELECT subject_id, cohort_start_date\n        FROM @cohort_schema.@cohort_table\n        {@cohort_definition_id != -999999} ? {WHERE cohort_definition_id = @cohort_definition_id} : {}\n      )\n    "

    # Optional observation_period constraint
    obs_join <- if (isTRUE(restrict_to_observation_period)) {
      paste0(
        " JOIN @cdm_schema.observation_period op\n            ON op.person_id = c.subject_id\n           AND d.@date_field BETWEEN op.observation_period_start_date AND op.observation_period_end_date "
      )
    } else {
      ""
    }

    sql_tmpl <- paste0(
      cohort_cte, "\n      SELECT COUNT(DISTINCT c.subject_id) AS patient_count\n      FROM cohort c\n      JOIN @cdm_schema.@domain_table d\n        ON d.@concept_field = @concept_id\n       AND d.@date_field BETWEEN DATEADD(day, @w_start, c.cohort_start_date)\n                             AND DATEADD(day, @w_end,   c.cohort_start_date)\n      ", obs_join
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
