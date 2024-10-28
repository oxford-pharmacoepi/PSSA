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
#' @param cohortDateRange Two dates indicating study period and the sequences that the user wants
#' to restrict to.
#' @param indexId Cohort definition IDs in indexTable to be considered for the analysis.
#' Change to NULL if all indices are wished to be included.
#' @param markerId Cohort definition IDs in markerTable to be considered for the analysis.
#' Change to NULL if all markers are wished to be included.
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
                              cohortDateRange = as.Date(c(NA, NA)),
                              indexId = NULL,
                              markerId = NULL,
                              daysPriorObservation = 0,
                              washoutWindow = 0,
                              indexMarkerGap = NULL,
                              combinationWindow = c(0,365),
                              movingAverageRestriction = 548){
  ### checks
  checkInputgenerateSequenceCohortSet(
    cdm = cdm,
    indexTable = indexTable,
    markerTable = markerTable,
    name = name,
    cohortDateRange = cohortDateRange,
    indexId = indexId,
    markerId = markerId,
    daysPriorObservation = daysPriorObservation,
    washoutWindow = washoutWindow,
    indexMarkerGap = indexMarkerGap,
    combinationWindow = combinationWindow,
    movingAverageRestriction = movingAverageRestriction
  )

  ### internal and exported parameters
  if (any(is.na(cohortDateRange))) {
    cohortDateRange <- getcohortDateRange(
      cdm = cdm,
      cohortDateRange = cohortDateRange
    )
  }

  cohort_date_range_1 <- cohortDateRange[[1]]
  cohort_date_range_2 <- cohortDateRange[[2]]

  comb_export_1 <- as.character(combinationWindow[[1]])
  comb_export_2 <- as.character(combinationWindow[[2]])

  combination_window <- combinationWindow

  if(!is.finite(combination_window[2])){
    combination_window[2] <- as.integer(99999)
  }

  moving_average_restriction <- movingAverageRestriction

  if(!is.finite(moving_average_restriction)){
    moving_average_restriction <- as.integer(99999)
  }

  index_marker_gap <- indexMarkerGap

  if (is.null(index_marker_gap)) {
    index_marker_gap <- combination_window[2]
    indexMarkerGap <- combinationWindow[2]
  }

  ### nsr
  nsr_name <- omopgenerics::uniqueId()
  nsr_index_name <- paste0(nsr_name, "_", indexTable, "_index")
  nsr_marker_name <- paste0(nsr_name, "_", markerTable, "_marker")
  nsr_summary_name <- paste0(nsr_name, "_nsr_summary")

  index_nsr_summary <- inc_cohort_summary(cdm = cdm,
                                          tableName = indexTable,
                                          cohortId = indexId,
                                          nsrTableName = nsr_index_name,
                                          cohortDateRange = cohortDateRange) |>
    dplyr::rename("index_cohort_definition_id" = "cohort_definition_id",
                  "index_n" = "n")

  if (nrow(index_nsr_summary |> dplyr::collect()) ==0){
    cli::cli_abort("Aborted! There are no events in the index cohort during the cohortDateRange specified. ")
  }

  marker_nsr_summary <- inc_cohort_summary(cdm = cdm,
                                           tableName = markerTable,
                                           cohortId = markerId,
                                           nsrTableName = nsr_marker_name,
                                           cohortDateRange = cohortDateRange) |>
    dplyr::rename("marker_cohort_definition_id" = "cohort_definition_id",
                  "marker_n" = "n")

  if (nrow(marker_nsr_summary |> dplyr::collect()) ==0){
    cli::cli_abort("Aborted! There are no events in the marker cohort during the cohortDateRange specified. ")
  }

  nsr_df <- index_nsr_summary |>
    dplyr::full_join(marker_nsr_summary,
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
    dplyr::pull(.data$index_cohort_definition_id)

  existing_marker_id <- nsr_df |>
    dplyr::filter(!is.na(.data$marker_cohort_definition_id)) |>
    dplyr::distinct(.data$marker_cohort_definition_id) |>
    dplyr::collect() |>
    dplyr::pull(.data$marker_cohort_definition_id)

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
        dplyr::collect() |>
        dplyr::mutate(
          marker_forward = deltaCumulativeSum(.data$marker_n, .data$cohort_start_date, moving_average_restriction, backwards = F),
          marker_backward = deltaCumulativeSum(.data$marker_n, .data$cohort_start_date, moving_average_restriction, backwards = T)
        ) |>
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

  ### Preprocess both cohorts
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

  time_1 <- combination_window[1]
  time_2 <- combination_window[2]

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

  ### Post-join processing
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
    dplyr::mutate(cohort_date_range = paste0("(",.env$cohort_date_range_1, ",",
                                             .env$cohort_date_range_2, ")"),
                  days_prior_observation = .env$daysPriorObservation,
                  washout_window = .env$washoutWindow,
                  index_marker_gap = .env$indexMarkerGap,
                  combination_window = paste0("(",.env$comb_export_1, ",",
                                              .env$comb_export_2, ")"),
                  moving_average_restriction = .env$movingAverageRestriction) |>
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

    ### exclusion criteria - where attrition starts
    # 1) within combination window
    cdm[[name]] <- cdm[[name]] |>
      dplyr::filter(abs(.data$gap) > .env$time_1 &
                      abs(.data$gap) <= .env$time_2) |>
      dplyr::compute(name = name, temporary = FALSE) |>
      omopgenerics::recordCohortAttrition(reason="Events excluded due to the prespecified combination window")

    # 2) indexMarkerGap
    cdm[[name]] <- cdm[[name]] |>
      dplyr::filter(.data$cei <= .env$index_marker_gap) |>
      dplyr::compute(name = name, temporary = FALSE) |>
      omopgenerics::recordCohortAttrition(reason="Events excluded due to the prespecified index marker gap")

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
  cdm <- CDMConnector::dropTable(cdm = cdm, name = dplyr::starts_with(nsr_name))

  return(cdm)
}

### extra functions
# If the user doesn't specify date range
# range to min and max of obs period
getcohortDateRange <- function(cdm, cohortDateRange) {
  if (is.na(cohortDateRange[1])) {
    cohortDateRange[1] <- as.Date(cdm[["observation_period"]] |>
      dplyr::summarise(
        min = min(.data$observation_period_start_date,
          na.rm = TRUE
        )
      ) |>
      dplyr::collect() |>
      dplyr::pull("min"))
  }
  if (is.na(cohortDateRange[2])) {
    cohortDateRange[2] <- as.Date(cdm[["observation_period"]] |>
      dplyr::summarise(
        max = max(.data$observation_period_end_date,
          na.rm = TRUE
        )
      ) |>
      dplyr::collect() |>
      dplyr::pull("max"))
  }
  return(cohortDateRange)
}

preprocessCohort <- function(cdm, cohortName, cohortId, cohortDateRange) {
  cohort <- cdm[[cohortName]]
  if (!is.null(cohortId)) {
    cohort <- cohort |>
      dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)
  }
  id <- "tmp_id_12345"
  nm <- paste0("tmp_001_",  omopgenerics::uniqueTableName())
  cohort <- cohort |>
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
    dplyr::arrange(.data$cohort_start_date) |>
    dplyr::mutate(!!id := dplyr::row_number()) |>
    dplyr::compute(name = nm, temporary = FALSE)
  cohort <- cohort |>
    dplyr::left_join(
      cohort |>
        dplyr::select(dplyr::all_of(
          c("previous_exposure" = "cohort_start_date", id, "cohort_definition_id", "subject_id")
        )) |>
        dplyr::mutate(!!id := .data[[id]] + 1),
      by = c(id, "cohort_definition_id", "subject_id")
    ) %>%
    dplyr::mutate(gap_to_prior = as.numeric(!!CDMConnector::datediff(
      "previous_exposure", "cohort_start_date"
    ))) |>
    dplyr::filter(
      .data$cohort_start_date <= !!cohortDateRange[[2]] &
        .data$cohort_start_date >= !!cohortDateRange[[1]]
    ) |>
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
    dplyr::filter(.data[[id]] == min(.data[[id]], na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(!dplyr::all_of(c(id, "previous_exposure"))) |>
    dplyr::compute(name = nm, temporary = FALSE) |>
    PatientProfiles::addCohortName() |>
    dplyr::compute()
  cdm <- omopgenerics::dropTable(cdm = cdm, name = nm)
  return(cohort)
}

inc_cohort_summary <- function(cdm, tableName, cohortId, nsrTableName, cohortDateRange){
  nsr_cohort <- cdm [[tableName]]
  if (!is.null(cohortId)) {
    nsr_cohort <- nsr_cohort |>
      dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)
  }
  nsr_cohort_summary <- nsr_cohort |>
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
    dplyr::arrange(.data$cohort_start_date) |>
    dplyr::mutate(row_num = dplyr::row_number()) |>
    dplyr::filter(.data$row_num == 1) |>
    dplyr::select(-"row_num") |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$cohort_definition_id, .data$cohort_start_date) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::filter(
      .data$cohort_start_date <= !!cohortDateRange[[2]] &
        .data$cohort_start_date >= !!cohortDateRange[[1]]
    ) |>
    dplyr::compute(name = nsrTableName, temporary = FALSE)
  return(nsr_cohort_summary)
}
