#' Sequence ratio calculations
#'
#' @description
#' Using generateSequenceCohortSet to obtain sequence ratios for the desired outcomes.
#'
#' @param cohort A cohort table in the cdm.
#' @param cohortId The Ids in the cohort that are to be included in the analyses.
#' @param confidenceInterval Default is 95, indicating the central 95% confidence interval.
#' @param minCellCount The minimum number of events to reported, below which
#' results will be obscured. If 0, all results will be reported.
#'
#' @return
#' A local table with all the analyses.
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
#' pssa_result <- summariseSequenceRatios(cohort = cdm$joined_cohorts, minCellCount = 0)
#' pssa_result
#' CDMConnector::cdmDisconnect(cdm)
#' }
#'
summariseSequenceRatios <- function(cohort,
                                    cohortId = NULL,
                                    confidenceInterval = 95,
                                    minCellCount = 5) {

  # checks
  cdm <- omopgenerics::cdmReference(cohort)
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort)
  omopgenerics::assertNumeric(confidenceInterval,
                              min = 1,
                              max = 99,
                              length = 1)
  omopgenerics::assertNumeric(minCellCount,
                              min = 0,
                              max = 99999999,
                              length = 1)

  if (is.null(cohortId)){
    cohortId <- cohort |>
      dplyr::select("cohort_definition_id") |>
      dplyr::distinct() |>
      dplyr::pull("cohort_definition_id")
  }

  cohort_tidy <- cohort |>
    dplyr::filter(.data$cohort_definition_id %in% cohortId) |>
    dplyr::left_join(omopgenerics::settings(cohort), copy = T, by = "cohort_definition_id") |>
    dplyr::compute()

  output <- data.frame()

  for (i in (cohort_tidy |> dplyr::distinct(.data$index_id) |> dplyr::pull())){
    for (j in (cohort_tidy |> dplyr::filter(.data$index_id == i) |> dplyr::distinct(.data$marker_id) |> dplyr::pull())){
      temporary_cohort <- cohort_tidy |>
        dplyr::filter(.data$index_id == i & .data$marker_id == j) |>
        dplyr::left_join(
          cohort_tidy |>
            dplyr::filter(.data$index_id == i & .data$marker_id == j) |>
            dplyr::group_by(.data$index_id, .data$marker_id) |>
            dplyr::summarise(date_start = min(.data$cohort_start_date, na.rm = T),
                             .groups = "drop") |>
            dplyr::ungroup(),
          by  = c("index_id", "marker_id")
        ) %>%
        dplyr::mutate(
          order_ba = .data$index_date >= .data$marker_date,
          days_first = as.numeric(!!CDMConnector::datediff(
            "date_start", "cohort_start_date"
          )), # gap between the first drug of a person and the first drug of the whole population
          days_second = as.numeric(!!CDMConnector::datediff(
            "cohort_start_date", "cohort_end_date"
          ))) |>
        dplyr::collect() |>
        dplyr::group_by(.data$days_first, .data$index_id, .data$index_name, .data$marker_id, .data$marker_name, .data$cohort_date_range, .data$days_prior_observation, .data$washout_window, .data$index_marker_gap, .data$combination_window, .data$moving_average_restriction) |>
        dplyr::summarise(marker_first = sum(.data$order_ba, na.rm = T), index_first = sum((!.data$order_ba), na.rm = T), .groups = "drop") |>
        dplyr::ungroup()

      csr <- crudeSequenceRatio(temporary_cohort)
      nsr <- omopgenerics::settings(cohort) |>
        dplyr::filter(.data$index_id == i & .data$marker_id == j) |>
        dplyr::pull("nsr")
      asr <- csr/nsr
      counts <- getConfidenceInterval(table = temporary_cohort,
                                      nsr = nsr,
                                      confidenceInterval = confidenceInterval) |>
        dplyr::select(-c("index_first", "marker_first"))

      meta_info <-
        temporary_cohort |>
        dplyr::group_by(.data$index_id, .data$index_name, .data$marker_id, .data$marker_name, .data$cohort_date_range, .data$days_prior_observation, .data$washout_window, .data$index_marker_gap, .data$combination_window, .data$moving_average_restriction) |>
        dplyr::summarise(marker_first = sum(.data$marker_first), index_first = sum(.data$index_first), .groups = "drop") |>
        dplyr::ungroup()

      partial_result <- cbind(meta_info,
                              cbind(tibble::tibble(csr = csr,asr = asr),
                                    counts)) |>
        dplyr::mutate(marker_first_percentage = round(.data$marker_first/(.data$marker_first + .data$index_first)*100, digits = 1),
                      index_first_percentage = round(.data$index_first/(.data$marker_first + .data$index_first)*100, digits = 1),
                      confidence_interval = as.character(.env$confidenceInterval)) |>
        dplyr::select("index_id", "index_name", "marker_id", "marker_name",
                      "index_first", "marker_first", "index_first_percentage", "marker_first_percentage",
                      "csr", "lower_csr_ci", "upper_csr_ci",
                      "asr", "lower_asr_ci", "upper_asr_ci", "cohort_date_range",
                      "days_prior_observation", "washout_window", "index_marker_gap", "combination_window",
                      "moving_average_restriction", "confidence_interval")
      output <- rbind(output, partial_result)
    }
  }

  ifp_100 <- output |>
    dplyr::filter(.data$index_first_percentage == 100) |>
    dplyr::tally() |>
    dplyr::pull("n")
  mfp_100 <- output |>
    dplyr::filter(.data$marker_first_percentage == 100) |>
    dplyr::tally() |>
    dplyr::pull("n")
  if(ifp_100 > 0 | mfp_100 > 0){
    cli::cli_warn("For at least some combinations, index is always before marker or marker always before index")
    if(ifp_100 > 0){
    cli::cli_inform("-- {ifp_100} combination{?s} of {nrow(output)} had index always before marker")
    }
    if(mfp_100 > 0){
      cli::cli_inform("-- {ifp_100} combination{?s}  of {nrow(output)} had marker always before index")
    }
    }

  output <- output |>
    PatientProfiles::addCdmName(cdm = omopgenerics::cdmReference(cohort)) |>
    getSummarisedResult()

 setting <- omopgenerics::settings(output)

  output <- output |>
    omopgenerics::suppress(minCellCount = minCellCount)

  counts <- output |>
    dplyr::filter(.data$estimate_name == "count" | .data$estimate_name == "percentage")

  output_sr <- output |> dplyr::filter(.data$variable_level == "sequence_ratio")

  index_count <- output |>
    dplyr::filter(!.data$variable_level == "sequence_ratio",
                  .data$variable_name == "index",
                  .data$estimate_name == "count") |>
    dplyr::rename("variable_name_index" = "variable_name",
                  "variable_level_index" = "variable_level",
                  "estimate_name_index" = "estimate_name",
                  "estimate_type_index" = "estimate_type",
                  "estimate_value_index" = "estimate_value")

  output_suppressed <- output_sr |>
    dplyr::left_join(index_count,
                     by = c("result_id", "cdm_name", "group_name",
                            "group_level", "strata_name", "strata_level",
                            "additional_name", "additional_level"),
                     relationship = "many-to-many") |>
    dplyr::mutate(estimate_value =
                    dplyr::case_when(
                      is.na(.data$estimate_value_index) ~ NA,
                      T ~ .data$estimate_value
                    )
    ) |>
    dplyr::select(dplyr::all_of(omopgenerics::resultColumns()))

  marker_count <- output |>
    dplyr::filter(!.data$variable_level == "sequence_ratio",
                  .data$variable_name == "marker",
                  .data$estimate_name == "count") |>
    dplyr::rename("variable_name_index" = "variable_name",
                  "variable_level_index" = "variable_level",
                  "estimate_name_index" = "estimate_name",
                  "estimate_type_index" = "estimate_type",
                  "estimate_value_index" = "estimate_value")

  output_suppressed <- output_suppressed |>
    dplyr::left_join(marker_count,
                     by = c("result_id", "cdm_name", "group_name",
                            "group_level", "strata_name", "strata_level",
                            "additional_name", "additional_level"),
                     relationship = "many-to-many") |>
    dplyr::mutate(estimate_value =
                    dplyr::case_when(
                      is.na(.data$estimate_value_index) ~ NA,
                      T ~ .data$estimate_value
                    )
    ) |>
    dplyr::select(dplyr::all_of(omopgenerics::resultColumns()))|>
    rbind(counts) |>
    dplyr::arrange(.data$group_level) |>
    omopgenerics::newSummarisedResult(
      settings = setting
    )

  return(output_suppressed)
}
