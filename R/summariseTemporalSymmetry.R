#' Summarise temporal symmetry
#'
#' @description
#' Using generateSequenceCohortSet to obtain temporal symmetry (aggregated
#' counts) of two cohorts.
#'
#' @param cohort A cohort table in the cdm.
#' @param cohortId The Ids in the cohort that are to be included in the analyses.
#' @param timescale Timescale for the x axis of the plot (month, day, year).
#' @param minCellCount The minimum number of events to reported, below which
#' results will be obscured. If 0, all results will be reported.
#'
#' @return
#' An aggregated table with difference in time (marker - index) and the relevant
#'  counts.
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortSymmetry)
#' cdm <- mockCohortSymmetry()
#' cdm <- generateSequenceCohortSet(cdm = cdm,
#'                                  name = "joined_cohorts",
#'                                  indexTable = "cohort_1",
#'                                  markerTable = "cohort_2")
#' temporal_symmetry <- summariseTemporalSymmetry(cohort = cdm$joined_cohorts, minCellCount = 0)
#' CDMConnector::cdmDisconnect(cdm)
#' }
#'
summariseTemporalSymmetry <- function(cohort,
                                      cohortId = NULL,
                                      timescale = "month",
                                      minCellCount = 5) {
  # checks
  checkInputSummariseTemporalSymmetry(cohort = cohort,
                                      cohortId = cohortId,
                                      timescale = timescale,
                                      minCellCount = minCellCount)

  index_names <- attr(cohort, "cohort_set") |>
    dplyr::select("cohort_definition_id", "index_name", "index_id", "marker_id")
  marker_names <- attr(cohort, "cohort_set") |>
    dplyr::select("cohort_definition_id", "marker_name")
  cohort_settings <- omopgenerics::settings(cohort)|>
    dplyr::mutate(timescale = .env$timescale) |>
    dplyr::select(-c("index_id", "marker_id", "index_name", "marker_name"))
  settings <- c("cohort_date_range", "days_prior_observation", "washout_window", "index_marker_gap",
                "combination_window", "moving_average_restriction", "timescale")

  output <- cohort %>%
    dplyr::mutate(time = as.numeric(!!CDMConnector::datediff(
      "index_date", "marker_date", interval = timescale))) |>
    dplyr::select("cohort_definition_id", "time") |>
    dplyr::group_by(.data$cohort_definition_id, .data$time) |>
    dplyr::summarise(count = as.integer(dplyr::n())) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      index_names,
      by = c("cohort_definition_id")
    ) |>
    dplyr::left_join(
      marker_names,
      by = c("cohort_definition_id")
    ) |>
    dplyr::compute()

  if(!is.null(cohortId)) {
    output <- output |>
      dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)
  }

  output_sum <- output |>
    PatientProfiles::addCdmName(cdm = omopgenerics::cdmReference(cohort)) |>
    dplyr::collect() |>
    dplyr::select(-c("index_id", "marker_id")) |>
    visOmopResults::uniteGroup(cols = c("index_name", "marker_name")) |>
    tidyr::pivot_longer(
      cols = c("time"),
      names_to = "additional_col",
      values_to = "variable_level"
    ) |>
    dplyr::select(-"additional_col") |>
    tidyr::pivot_longer(
      cols = c("count"),
      names_to = "estimate_name",
      values_to = "estimate_value"
    ) |>
    dplyr::mutate(variable_name  = "temporal_symmetry",
                  variable_level = as.character(.data$variable_level),
                  estimate_value = as.character(.data$estimate_value),
                  strata_name = "overall",
                  strata_level = "overall",
                  additional_name = "overall",
                  additional_level = "overall",
                  estimate_type =
                    dplyr::case_when(
                      (.data$estimate_name == "count") ~ "integer"
                    )) |>
    dplyr::inner_join(cohort_settings, by = "cohort_definition_id") |>
    dplyr::select(c(-"cohort_name", -"cohort_definition_id"))

  setting <- output_sum |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(settings, "cdm_name")))) |>
    dplyr::mutate(result_id = as.integer(dplyr::row_number()),
                  result_type = "temporal_symmetry",
                  package_name = "CohortSymmetry",
                  package_version = as.character(utils::packageVersion("CohortSymmetry")),
                  timescale = .env$timescale)

  output_sum <- output_sum |>
    dplyr::left_join(setting, by = c("cdm_name", "days_prior_observation", "washout_window",
                                     "index_marker_gap", "combination_window", "timescale")) |>
    dplyr::select(dplyr::all_of(omopgenerics::resultColumns())) |>
    omopgenerics::newSummarisedResult(
      settings = setting
    ) |>
    omopgenerics::suppress(minCellCount = minCellCount)
  return(output_sum)
}
