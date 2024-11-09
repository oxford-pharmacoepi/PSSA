getSummarisedResult <- function(x) {
  settings <- c("cohort_date_range", "days_prior_observation", "washout_window", "index_marker_gap",
                "combination_window", "moving_average_restriction", "confidence_interval")
  x_sum <- x |>
    dplyr::mutate(
      group_name = "index_cohort_name &&& marker_cohort_name",
      group_level = paste0(.data$index_name, " &&& ", .data$marker_name),
      strata_name = "overall", # to update
      strata_level = "overall", # to update
    ) |>
    dplyr::rename(
      "index_first_count" = "index_first",
      "marker_first_count" = "marker_first"
    ) |>
    tidyr::pivot_longer(
      cols = c("index_first_count", "index_first_percentage",
               "marker_first_count", "marker_first_percentage",
               "csr", "asr", "lower_csr_ci", "upper_csr_ci", "lower_asr_ci",
               "upper_asr_ci"),
      names_to = "variable_level",
      values_to = "estimate_value"
    ) |>
    dplyr::mutate(
      variable_name = dplyr::case_when(
        grepl("csr", .data$variable_level, ignore.case = TRUE) ~ "crude",
        grepl("asr", .data$variable_level, ignore.case = TRUE) ~ "adjusted",
        grepl("index", .data$variable_level) ~ "index",
        grepl("marker", .data$variable_level) ~ "marker"
      ),
      estimate_name = dplyr::case_when(
        .data$variable_level %in% c("csr", "asr") ~ "point_estimate",
        grepl("lower", .data$variable_level) ~ "lower_CI",
        grepl("upper", .data$variable_level) ~ "upper_CI",
        grepl("count", .data$variable_level) ~ "count",
        grepl("percentage", .data$variable_level) ~ "percentage"
      ),
      estimate_type = dplyr::if_else(
        grepl("count", .data$variable_level),
        "integer", "numeric"),
      variable_level = dplyr::if_else(
        .data$variable_level %in%  c("csr", "asr", "lower_csr_ci", "upper_csr_ci",
                                    "lower_asr_ci", "upper_asr_ci"),
        "sequence_ratio", "first_pharmac"
      ),
      estimate_value = as.character(.data$estimate_value),
      additional_name = "overall",
      additional_level = "overall"
    )

  setting <- x |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(settings, "cdm_name")))) |>
    dplyr::mutate(result_id = as.integer(dplyr::row_number()),
                  result_type = "sequence_ratios",
                  package_name = "CohortSymmetry",
                  package_version = as.character(utils::packageVersion("CohortSymmetry")))

  x_sum <- x_sum |>
    dplyr::left_join(setting, by = c("days_prior_observation", "washout_window",
                     "index_marker_gap", "combination_window", "moving_average_restriction", "confidence_interval",
                     "cdm_name")) |>
    dplyr::select(dplyr::all_of(omopgenerics::resultColumns())) |>
    omopgenerics::newSummarisedResult(
      settings = setting
    )

  return(x_sum)
}
