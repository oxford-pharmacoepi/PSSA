#' A formatted visualization of temporal_symmetry objects.
#'
#' @description
#' It provides a formatted table with the contents of the summariseTemporalSymmetry
#' output.
#'
#' @param result A temporal_symmetry object.
#' @param header A vector specifying the elements to include in the header.
#' See visOmopResults package for more information on how to use this parameter.
#' @param groupColumn Columns to use as group labels.
#' See visOmopResults package for more information on how to use this parameter.
#' @param type The desired format of the output table.
#' @param hide Columns to drop from the output table.
#'
#' @return A formatted version of the temporal_symmetry object.
#'
#' @export
#' @examples
#' \donttest{
#' library(CohortSymmetry)
#' cdm <- mockCohortSymmetry()
#' cdm <- generateSequenceCohortSet(cdm = cdm,
#'                                  indexTable = "cohort_1",
#'                                  markerTable = "cohort_2",
#'                                  name = "joined_cohort")
#' res <- summariseTemporalSymmetry(cohort = cdm$joined_cohort, minCellCount = 0)
#' gtResult <- tableTemporalSymmetry(result = res)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
tableTemporalSymmetry <- function(result,
                                  header = "variable_level",
                                  groupColumn = c("cdm_name", "index_name"),
                                  type = "gt",
                                  hide = "variable_name"){

  rlang::check_installed("flextable")
  rlang::check_installed("gt")

  # validate checks
  result <- omopgenerics::validateResultArgument(result)

  # check settings
  result <- result |>
    visOmopResults::filterSettings(
      .data$result_type == "temporal_symmetry"
    )

  if (nrow(result) == 0) {
    cli::cli_warn("`result` object does not contain any `result_type == 'temporal_symmetry'` information.")
    return(emptyResultTable(type))
  }

  setting <- omopgenerics::settings(result)

  result_visualised <- result |>
    omopgenerics::addSettings() |>
    dplyr::mutate(estimate_name = paste0(.data$timescale, "ly_count"),
                  variable_level = as.integer(.data$variable_level)) |>
    dplyr::group_by(.data$group_level) |>
    dplyr::arrange(.data$variable_level) |>
    dplyr::ungroup() |>
    dplyr::mutate(variable_level = as.character(.data$variable_level))

  result <- result_visualised |>
    dplyr::select(dplyr::all_of(omopgenerics::resultColumns())) |>
    omopgenerics::newSummarisedResult(
      settings = setting
    )

  # format table
  tab <- visOmopResults::visOmopTable(
    result = result,
    header = header,
    groupColumn = groupColumn,
    type = type,
    hide = hide
  )
}
