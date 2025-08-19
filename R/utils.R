#' Get Supported OMOP Domains
#'
#' @return Character vector of supported OMOP domains
#'
#' @export
get_supported_domains <- function() {
  return(c("Procedure", "Measurement", "Condition", "Drug", "Device", "Observation"))
}

#' Create Summary Statistics
#'
#' @param results Data frame with prevalence analysis results
#' @return Data frame with summary statistics
#'
#' @export
create_summary_stats <- function(results) {
  summary_stats <- results %>%
    dplyr::group_by(cohortname, omop_object_domain, workflow_stage) %>%
    dplyr::summarise(
      total_tests = dplyr::n(),
      avg_prevalence = round(mean(patient_count, na.rm = TRUE), 2),
      median_prevalence = round(median(patient_count, na.rm = TRUE), 2),
      min_prevalence = min(patient_count, na.rm = TRUE),
      max_prevalence = max(patient_count, na.rm = TRUE),
      total_patients = max(n_patients, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(summary_stats)
}

#' Export Results to Multiple Formats
#'
#' @param results Data frame with analysis results
#' @param output_prefix Character. Prefix for output files
#' @param formats Character vector. Output formats ("tsv", "csv", "xlsx")
#'
#' @export
export_results <- function(results, output_prefix = "prevalence_analysis", formats = c("tsv")) {
  
  if ("tsv" %in% formats) {
    tsv_path <- paste0(output_prefix, ".tsv")
    readr::write_tsv(results, tsv_path)
    cat("TSV results written to:", tsv_path, "\n")
  }
  
  if ("csv" %in% formats) {
    csv_path <- paste0(output_prefix, ".csv")
    readr::write_csv(results, csv_path)
    cat("CSV results written to:", csv_path, "\n")
  }
  
  if ("xlsx" %in% formats && requireNamespace("openxlsx", quietly = TRUE)) {
    xlsx_path <- paste0(output_prefix, ".xlsx")
    openxlsx::write.xlsx(results, xlsx_path)
    cat("Excel results written to:", xlsx_path, "\n")
  }
}

#' Create Prevalence Report
#'
#' @param results Data frame with analysis results
#' @param output_path Character. Path for the report file
#'
#' @export
create_prevalence_report <- function(results, output_path = "prevalence_report.txt") {
  
  # Create summary statistics
  summary_stats <- create_summary_stats(results)
  
  # Generate report content
  report_lines <- c(
    "=== PREVALENCE ANALYSIS REPORT ===",
    paste("Generated on:", Sys.time()),
    "",
    "=== OVERALL SUMMARY ===",
    paste("Total combinations analyzed:", nrow(results)),
    paste("Unique cohorts:", length(unique(results$cohortname))),
    paste("Unique domains:", length(unique(results$omop_object_domain))),
    paste("Unique workflow stages:", length(unique(results$workflow_stage))),
    "",
    "=== DOMAIN BREAKDOWN ===",
    ""
  )
  
  # Add domain-specific summaries
  domain_summary <- results %>%
    dplyr::group_by(omop_object_domain) %>%
    dplyr::summarise(
      count = dplyr::n(),
      avg_prevalence = round(mean(patient_count, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  for (i in 1:nrow(domain_summary)) {
    domain_line <- paste0(
      domain_summary$omop_object_domain[i], ": ",
      domain_summary$count[i], " tests, ",
      "avg prevalence: ", domain_summary$avg_prevalence[i], "%"
    )
    report_lines <- c(report_lines, domain_line)
  }
  
  report_lines <- c(report_lines, "", "=== DETAILED SUMMARY BY COHORT AND DOMAIN ===", "")
  
  # Add detailed summary
  for (i in 1:nrow(summary_stats)) {
    detail_line <- paste0(
      "Cohort: ", summary_stats$cohortname[i],
      " | Domain: ", summary_stats$omop_object_domain[i],
      " | Stage: ", summary_stats$workflow_stage[i],
      " | Tests: ", summary_stats$total_tests[i],
      " | Avg Prevalence: ", summary_stats$avg_prevalence[i], "%",
      " | Patients: ", summary_stats$total_patients[i]
    )
    report_lines <- c(report_lines, detail_line)
  }
  
  # Write report to file
  writeLines(report_lines, output_path)
  cat("Report written to:", output_path, "\n")
}