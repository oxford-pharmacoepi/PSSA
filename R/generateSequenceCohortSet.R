#' Intersecting the index and marker cohorts prior to calculating Sequence Symmetry Ratios
#'
#' @description
#' Join two tables in the CDM (one for index and the other for marker cohorts)
#' into a new table in the cdm taking into account the maximum time interval between events.
#' Index and marker cohorts should be instantiated in advance by the user.
#'
#' @param cdm A CDM reference.
#' @param indexTable A table in the CDM that the index cohorts should come from.
#' @param markerTable A table in the CDM that the marker cohorts should come from.
#' @param name The name within the cdm that the output is called. Default is joined_cohorts.
#' @param indexId Cohort definition IDs in indexTable to be considered for the analysis.
#' Change to NULL if all indices are wished to be included.
#' @param markerId Cohort definition IDs in markerTable to be considered for the analysis.
#' Change to NULL if all markers are wished to be included.
#' @param cohortDateRange Two dates indicating study period and the sequences that the user wants
#' to restrict to.
#' @param daysPriorObservation The minimum amount of prior observation required on both the index
#' and marker cohorts per person.
#' @param washoutWindow A washout window to be applied on both the index cohort event and marker cohort.
#' @param indexMarkerGap The maximum allowable gap between the end of the first episode
#' and the start of the second episode in a sequence/combination.
#' @param combinationWindow A constrain to be placed on the gap between two initiations.
#' Default c(0,365), meaning the gap should be larger than 0 but less than or equal to 365.
#' @param movingAverageRestriction The moving window when calculating nSR, default is 548.
#'
#' @return
#' A table within the cdm reference.
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortSymmetry)
#' cdm <- mockCohortSymmetry()
#' cdm <- generateSequenceCohortSet(
#'   cdm = cdm,
#'   name = "joined_cohorts",
#'   indexTable = "cohort_1",
#'   markerTable = "cohort_2"
#' )
#'  cdm$joined_cohorts
#'  CDMConnector::cdmDisconnect(cdm = cdm)
#' }
generateSequenceCohortSet <- function(cdm,
                                      indexTable,
                                      markerTable,
                                      name,
                                      indexId = NULL,
                                      markerId = NULL,
                                      cohortDateRange = as.Date(c(NA, NA)),
                                      daysPriorObservation = 0,
                                      washoutWindow = 0,
                                      indexMarkerGap = Inf,
                                      combinationWindow = c(0,365),
                                      movingAverageRestriction = 548){
  # checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertCharacter(indexTable, length = 1)
  omopgenerics::assertCharacter(markerTable, length = 1)
  cdm[[indexTable]] <- omopgenerics::validateCohortArgument(cohort  = cdm[[indexTable]])
  cdm[[markerTable]] <- omopgenerics::validateCohortArgument(cohort = cdm[[markerTable]])
  name <- omopgenerics::validateNameArgument(name = name, validation = "warning")
  indexId <- omopgenerics::validateCohortIdArgument({{indexId}}, cdm[[indexTable]])
  markerId <- omopgenerics::validateCohortIdArgument({{markerId}}, cdm[[markerTable]])
  omopgenerics::assertNumeric(daysPriorObservation, min = 0, max = 999999)
  omopgenerics::assertNumeric(washoutWindow, min = 0, max = 999999)
  omopgenerics::assertNumeric(indexMarkerGap, min = 0)
  omopgenerics::assertNumeric(combinationWindow, length = 2)
  combinationWindow <- omopgenerics::validateWindowArgument(window = combinationWindow, snakeCase = TRUE)
  omopgenerics::assertNumeric(movingAverageRestriction, min = 0)

  # Change CohortDateRange
  if (any(is.na(cohortDateRange))) {
    cohortDateRange <- getcohortDateRange(
      cdm = cdm,
      cohortDateRange = cohortDateRange
    )
  }

  omopgenerics::assertDate(cohortDateRange, length = 2, unique = TRUE)

  # nsr
  nsr_name <- omopgenerics::uniqueId()
  nsr_summary_name <- paste0(nsr_name, "_summary")

  index_res <- inc_cohort_check(cdm = cdm,
                                tableName = indexTable,
                                cohortId = indexId,
                                nsrTableName = nsr_name,
                                cohortDateRange = cohortDateRange)

  if (length(index_res)>0){
    cli::cli_abort("Aborted! cohort_definition_id {index_res} in the index
                   cohort have no events during the cohortDateRange specified.")
  }

  marker_res <- inc_cohort_check(cdm = cdm,
                                 tableName = markerTable,
                                 cohortId = markerId,
                                 nsrTableName = nsr_name,
                                 cohortDateRange = cohortDateRange)

  if (length(marker_res)>0){
    cli::cli_abort("Aborted! cohort_definition_id {marker_res} in the index
                   cohort have no events during the cohortDateRange specified.")
  }

  index_nsr_summary <- inc_cohort_summary(cdm = cdm,
                                          tableName = indexTable,
                                          cohortId = indexId,
                                          nsrTableName = nsr_name,
                                          cohortDateRange = cohortDateRange)

  marker_nsr_summary <- inc_cohort_summary(cdm = cdm,
                                           tableName = markerTable,
                                           cohortId = markerId,
                                           nsrTableName = nsr_name,
                                           cohortDateRange = cohortDateRange)

  nsr_df <- index_nsr_summary |>
    dplyr::rename(
      "index_cohort_definition_id" = "cohort_definition_id",
      "index_n" = "n"
    ) |>
    dplyr::full_join(marker_nsr_summary |>
                       dplyr::rename(
                         "marker_cohort_definition_id" = "cohort_definition_id",
                         "marker_n" = "n"
                       ),
                     by = "cohort_start_date",
                     relationship = "many-to-many") |>
    dplyr::select(
      "index_cohort_definition_id",
      "marker_cohort_definition_id",
      "cohort_start_date",
      "index_n",
      "marker_n"
    ) |>
    dplyr::compute(name = nsr_summary_name, temporary = FALSE)

  nsr_calc <- list()
  existing_index_id <- nsr_df |>
    dplyr::filter(!is.na(.data$index_cohort_definition_id)) |>
    dplyr::distinct(.data$index_cohort_definition_id) |>
    dplyr::collect() |>
    dplyr::arrange(.data$index_cohort_definition_id) |>
    dplyr::pull("index_cohort_definition_id")

  existing_marker_id <- nsr_df |>
    dplyr::filter(!is.na(.data$marker_cohort_definition_id)) |>
    dplyr::distinct(.data$marker_cohort_definition_id) |>
    dplyr::collect() |>
    dplyr::arrange(.data$marker_cohort_definition_id) |>
    dplyr::pull("marker_cohort_definition_id")

  for (i in existing_index_id){
    for (j in existing_marker_id){
      nsr_calc_df <- nsr_df |>
        dplyr::filter(is.na(.data$index_cohort_definition_id) | .data$index_cohort_definition_id == i) |>
        dplyr::filter(is.na(.data$marker_cohort_definition_id) | .data$marker_cohort_definition_id == j) |>
        dplyr::mutate(
          index_n = dplyr::case_when(
            is.na(.data$index_n) ~ 0,
            T ~ index_n
          ),
          marker_n = dplyr::case_when(
            is.na(.data$marker_n) ~ 0,
            T ~ marker_n
          )
        ) |>
        dplyr::select("cohort_start_date", "index_n", "marker_n") |>
        dplyr::collect() %>%
        {if (is.infinite(movingAverageRestriction))
          dplyr::mutate(.,
            marker_forward = deltaCumulativeSum(.data$marker_n, .data$cohort_start_date, 99999, backwards = F),
            marker_backward = deltaCumulativeSum(.data$marker_n, .data$cohort_start_date, 99999, backwards = T)
          ) else
          dplyr::mutate(.,
            marker_forward = deltaCumulativeSum(.data$marker_n, .data$cohort_start_date, movingAverageRestriction, backwards = F),
            marker_backward = deltaCumulativeSum(.data$marker_n, .data$cohort_start_date, movingAverageRestriction, backwards = T)
          )
        } |>
        dplyr::mutate(im_forward = .data$index_n * .data$marker_forward,
                      im_backward = .data$index_n * .data$marker_backward)

      numerator <- sum(nsr_calc_df$im_forward)
      denominator <- sum(nsr_calc_df$im_forward) + sum(nsr_calc_df$im_backward)

      pa <- numerator/denominator
      nsr <- pa/(1-pa)

      nsr_calc[[paste0("index_", i, "_marker_", j)]] <-
        tibble::tibble(index_id = i, marker_id = j, nsr = nsr)
    }
  }

  nsr_tbl <- Reduce(dplyr::union_all, nsr_calc)

  # Preprocess both cohorts
  indexPreprocessed <- preprocessCohort(cdm = cdm, cohortName = indexTable,
                                        cohortId = indexId, cohortDateRange = cohortDateRange) |>
    dplyr::rename("index_id" = "cohort_definition_id",
                  "index_name" = "cohort_name",
      "index_date" = "cohort_start_date",
      "index_end_date" = "cohort_end_date",
      "gap_to_prior_index" = "gap_to_prior"
    )
  markerPreprocessed <- preprocessCohort(cdm = cdm, cohortName = markerTable,
                                         cohortId = markerId, cohortDateRange = cohortDateRange) |>
    dplyr::rename("marker_id" = "cohort_definition_id",
                  "marker_name" = "cohort_name",
                  "marker_date" = "cohort_start_date",
                  "marker_end_date" = "cohort_end_date",
                  "gap_to_prior_marker" = "gap_to_prior")

  joinedData <- indexPreprocessed |>
    dplyr::inner_join(
      markerPreprocessed,
      by = "subject_id"
    ) |>
    dplyr::mutate(
      first_date = dplyr::if_else(.data$index_date <= .data$marker_date,
                                                    .data$index_date,
                                                    .data$marker_date
    ),
    second_date = dplyr::if_else(.data$index_date >= .data$marker_date,
                                 .data$index_date,
                                 .data$marker_date)
    ) |>
    dplyr::inner_join(
      cdm[["observation_period"]], by = c("subject_id" = "person_id")
    ) |>
    dplyr::filter(.data$first_date >= .data$observation_period_start_date,
                  .data$second_date <= .data$observation_period_end_date) |>
    dplyr::select("index_id", "subject_id", "index_date",
                  "index_end_date", "gap_to_prior_index",
                  "index_name", "marker_id", "marker_date",
                  "marker_end_date", "gap_to_prior_marker",
                  "marker_name", "first_date", "second_date")

  # Post-join processing
  cdm[[name]] <- joinedData %>%
    dplyr::mutate(
      gap = as.numeric(!!CDMConnector::datediff("index_date", "marker_date",
                                                interval = "day")),
      gap_index_marker = as.numeric(!!CDMConnector::datediff("index_end_date", "marker_date",
                                        interval = "day")),
      gap_marker_index = as.numeric(!!CDMConnector::datediff("marker_end_date", "index_date",
                                                             interval = "day"))) |>
    dplyr::mutate(
      cei = dplyr::if_else(.data$index_date < .data$marker_date,
                           .data$gap_index_marker, .data$gap_marker_index)
    ) |>
    dplyr::select("index_id", "index_name",
                  "marker_id", "marker_name",
                  "subject_id", "index_date",
                  "marker_date", "first_date", "second_date",
                  "cei",
                  "gap_to_prior_index",
                  "gap_to_prior_marker", "gap")  |>
    dplyr::compute(name = name,
                   temporary = FALSE)

  cdm[["ids"]] <- cdm[[name]] |>
    dplyr::select(.data$index_id, .data$marker_id) |>
    dplyr::distinct() |>
    dplyr::arrange(.data$index_id, .data$marker_id) |>
    dplyr::mutate(cohort_definition_id = as.integer(dplyr::row_number())) |>
    dplyr::compute(name = "ids", temporary = FALSE)

  cdm[[name]] <- cdm[[name]] |>
    dplyr::left_join(cdm[["ids"]], by = c("index_id", "marker_id")) |>
    dplyr::mutate(cohort_start_date = .data$first_date,
                  cohort_end_date = .data$second_date,
                  cohort_name = paste0("index_", .data$index_name,
                                       "_marker_", .data$marker_name)) |>
    dplyr::select("cohort_definition_id", "index_id", "marker_id",
                  "cohort_name", "index_name", "marker_name",
                  "subject_id",
                  "cohort_start_date",
                  "cohort_end_date",
                  "index_date",
                  "marker_date",
                  "cei",
                  "gap_to_prior_index", "gap_to_prior_marker", "gap") |>
    PatientProfiles::addPriorObservation() |>
    dplyr::compute(name = name,
                   temporary = FALSE)

  cohortSetRef <- cdm[[name]]  |>
    dplyr::select("cohort_definition_id", "cohort_name", "index_id",
                  "index_name", "marker_id", "marker_name") |>
    dplyr::group_by(.data$cohort_definition_id, .data$cohort_name, .data$index_id,
                    .data$index_name, .data$marker_id, .data$marker_name) |>
    dplyr::distinct() |>
    dplyr::mutate(cohort_date_range = !!paste0("(",cohortDateRange[[1]], ",",
                                             cohortDateRange[[2]], ")"),
                  days_prior_observation = !!format(daysPriorObservation, nsmall = 0),
                  washout_window = !!format(washoutWindow, nsmall = 0),
                  index_marker_gap = !!format(indexMarkerGap, nsmall = 0),
                  combination_window = !!paste0("(",combinationWindow[[1]][1], ",",
                                                combinationWindow[[1]][2], ")"),
                  moving_average_restriction = !!format(movingAverageRestriction, nsmall = 0)) |>
    dplyr::left_join(nsr_tbl,
                     by = c("index_id", "marker_id"),
                     copy = T)

  cdm[[name]] <- cdm[[name]] |>
    dplyr::select(!c("cohort_name", "index_name", "marker_name"))

  if (cdm[[name]] |>
      dplyr::summarise(n = dplyr::n_distinct(.data$cohort_definition_id)) |>
      dplyr::pull("n") == 0) {
    cdm <- omopgenerics::emptyCohortTable(
      cdm = cdm,
      name = name
    )
  } else {
    cdm[[name]] <- cdm[[name]] |>
      dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date",
                    "cohort_end_date", "index_id", "marker_id", "index_date", "marker_date",
                    "cei", "gap_to_prior_index", "gap_to_prior_marker", "gap", "prior_observation") |>
      omopgenerics::newCohortTable(cohortSetRef = cohortSetRef,
                                   cohortAttritionRef = NULL)

    # exclusion criteria - where attrition starts
    # 1) within combination window
    cdm[[name]] <- cdm[[name]] %>%
      {if (is.infinite(combinationWindow[[1]][2]))
        dplyr::filter(.,
                      abs(.data$gap) > !!combinationWindow[[1]][1])
        else
        dplyr::filter(.,
                      abs(.data$gap) > !!combinationWindow[[1]][1] &
                      abs(.data$gap) <= !!combinationWindow[[1]][2])
        } |>
      dplyr::compute(name = name, temporary = FALSE) |>
      omopgenerics::recordCohortAttrition(reason="Events excluded due to the prespecified combination window")

    # 2) indexMarkerGap
    if(is.infinite(indexMarkerGap)){
      cdm[[name]] <- cdm[[name]] |>
        dplyr::compute(name = name, temporary = FALSE) |>
        omopgenerics::recordCohortAttrition(reason="Events excluded due to the prespecified index marker gap")
    } else {
      cdm[[name]] <- cdm[[name]] |>
        dplyr::filter(.data$cei <= .env$indexMarkerGap) |>
        dplyr::compute(name = name, temporary = FALSE) |>
        omopgenerics::recordCohortAttrition(reason="Events excluded due to the prespecified index marker gap")
    }

    # 3) days prior observation
    cdm[[name]] <- cdm[[name]] |>
      dplyr::filter(
        .data$prior_observation >= .env$daysPriorObservation
      ) |>
      dplyr::compute(name = name, temporary = FALSE) |>
      omopgenerics::recordCohortAttrition(reason="Events excluded due to insufficient prior history")

    # 4) washoutWindow
    cdm[[name]] <- cdm[[name]] |>
      dplyr::filter(
        .data$gap_to_prior_index >= .env$washoutWindow | is.na(.data$gap_to_prior_index),
        .data$gap_to_prior_marker >= .env$washoutWindow | is.na(.data$gap_to_prior_marker)
      ) |>
      dplyr::compute(name = name, temporary = FALSE) |>
      omopgenerics::recordCohortAttrition(reason="Events excluded due to insufficient washout window")

    # final output table
    cdm[[name]] <- cdm[[name]] |>
      dplyr::select("cohort_definition_id", "subject_id",
                    "cohort_start_date", "cohort_end_date",
                    "index_date", "marker_date")  |>
      dplyr::compute(name = name,
                     temporary = FALSE)
  }

  cdm <- CDMConnector::dropTable(cdm = cdm, name = "ids")
  CDMConnector::dropTable(cdm = cdm, name = dplyr::starts_with(nsr_name))

  return(cdm)
}
