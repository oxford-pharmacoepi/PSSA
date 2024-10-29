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

# to resolve "All declared Imports should be used"
redundant_fun <- function() {
  here::here()
  CodelistGenerator::mockVocabRef()
  cdm <- DrugUtilisation::mockDrugUtilisation()
  data <- cdm$cohort1 |> dplyr::collect()
  flextable::flextable(data)
  gt::gt(data)
  CDMConnector::cdmDisconnect(cdm = cdm)
}
