#' A plot for the sequence ratios.
#'
#' @description
#' It provides a ggplot of the sequence ratios of index and marker cohorts.
#'
#' @param result Table output from summariseSequenceRatios.
#' @param onlyASR If set to be TRUE then only adjusted SR will be plotted.
#' Otherwise if it is set to be FALSE then both adjusted and crude SR will be plotted.
#' @param plotTitle Title of the plot, if NULL no title will be included in the plot.
#' @param labs Axis labels for the plot.
#' @param colours Colours for sequence ratio.
#' @param facet The variable to facet by.
#'
#' @return A plot for the sequence ratios of index and marker cohorts.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortSymmetry)
#' cdm <- mockCohortSymmetry()
#' cdm <- generateSequenceCohortSet(cdm = cdm,
#'                                  indexTable = "cohort_1",
#'                                  markerTable = "cohort_2",
#'                                  name = "joined_cohort")
#' sequence_ratio <- summariseSequenceRatios(cohort = cdm$joined_cohort,
#'                                           minCellCount = 0)
#' plotSequenceRatios(result = sequence_ratio)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
plotSequenceRatios <- function(result,
                               onlyASR = FALSE,
                               plotTitle = NULL,
                               labs = c("SR", "Drug Pairs"),
                               colours = c("red", "blue"),
                               facet = NULL
                               ){

  rlang::check_installed("ggplot2")

  # validate checks
  result <- omopgenerics::validateResultArgument(result)

  # check settings
  result <- result |>
    visOmopResults::filterSettings(
      .data$result_type == "sequence_ratios"
    )

  if (nrow(result) == 0) {
    cli::cli_warn("`result` object does not contain any `result_type == 'sequence_ratios'` information.")
  }

  if (!is.logical(onlyASR)) {
    cli::cli_abort("The parameter onlyASR has to be either True or False.")
  }

  if(onlyASR) {
    checkmate::assert_character(colours,
                                len = 1)
  } else {
    checkmate::assert_character(colours,
                                len = 2)
  }

  data <- result |>
    omopgenerics::tidy() |>
    dplyr::mutate(
      pair = paste0(.data$index_cohort_name, "->", .data$marker_cohort_name)
    ) |>
    dplyr::filter(.data$variable_level == "sequence_ratio") |>
    dplyr::select("pair", "variable_name", "point_estimate",
                  "lower_CI", "upper_CI", "cdm_name",
                  "cohort_date_range", "combination_window",
                  "confidence_interval", "days_prior_observation",
                  "index_marker_gap", "moving_average_restriction",
                  "washout_window")

  if (onlyASR){
    data <- data |>
      dplyr::filter(.data$variable_name == "adjusted")

    custom_colors <- c("adjusted" = colours)

    p <- visOmopResults::scatterPlot(
      data,
      x = "pair",
      y = "point_estimate",
      line = FALSE,
      point = TRUE,
      ribbon = FALSE,
      ymin = "lower_CI",
      ymax = "upper_CI",
      facet = facet,
      colour = "variable_name"
    ) +
      ggplot2::ylab(labs[1]) +
      ggplot2::xlab(labs[2]) +
      ggplot2::labs(title = plotTitle) +
      ggplot2::coord_flip() +
      ggplot2::theme_bw() +
      ggplot2::scale_colour_manual(values = custom_colors) +
      ggplot2::theme(panel.border = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(),
                     legend.title = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5))

  } else {

    custom_colors <- c("adjusted" = colours[1],
                       "crude" = colours[2])

    p <- visOmopResults::scatterPlot(
      data,
      x = "pair",
      y = "point_estimate",
      line = FALSE,
      point = TRUE,
      ribbon = FALSE,
      ymin = "lower_CI",
      ymax = "upper_CI",
      facet = facet,
      colour = "variable_name"
    ) +
      ggplot2::ylab(labs[1]) +
      ggplot2::xlab(labs[2]) +
      ggplot2::labs(title = plotTitle) +
      ggplot2::coord_flip() +
      ggplot2::theme_bw() +
      ggplot2::scale_colour_manual(values = custom_colors) +
      ggplot2::theme(panel.border = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(),
                     legend.title = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5))
  }
  return(p)
}
