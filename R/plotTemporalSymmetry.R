#' A plot for the temporal symmetry of cohorts.
#'
#' @description
#' It provides a ggplot of the temporal symmetry of two or more cohorts.
#'
#' @param result Table output from summariseTemporalSymmetry.
#' @param plotTitle Title of the plot, if NULL no title will be plotted.
#' @param labs Axis labels for the plot.
#' @param xlim Limits for the x axis of the plot.
#' @param colours Colours for both parts of the plot, pre- and post- time 0.
#' @param scales Whether to set free y scales for the facet wrap when there are
#' multiple plots (i.e. each plot has its own scaled y axis) or set them equal
#' for all. Only accepts "free" for the former and "fixed" for the latter.
#'
#' @return A plot for the temporal symmetry of cohorts.
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
#' temporal_symmetry <- summariseTemporalSymmetry(cohort = cdm$joined_cohort,
#'                                                minCellCount = 0)
#' plotTemporalSymmetry(result = temporal_symmetry)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
plotTemporalSymmetry <- function(result,
                                 plotTitle = NULL,
                                 labs = c("Time (months)", "Individuals (N)"),
                                 xlim = c(-12, 12),
                                 colours = c("blue", "red"),
                                 scales = "free") {

  rlang::check_installed("ggplot2")

  # checks
  result <- omopgenerics::validateResultArgument(result = result)
  omopgenerics::assertCharacter(plotTitle, length = 1, null = T)
  omopgenerics::assertCharacter(labs, length = 2)
  omopgenerics::assertNumeric(xlim, length = 2, unique = T)
  omopgenerics::assertCharacter(colours, length = 2)
  scales <- omopgenerics::assertChoice(scales,
                                       choices = c("free", "fixed"))

  plot_data <- result |>
    visOmopResults::splitGroup() |>
    dplyr::select(.data$index_name, .data$marker_name, .data$variable_name, .data$variable_level, .data$estimate_name, .data$estimate_value, .data$additional_level, .data$additional_name) |>
    dplyr::group_by(.data$estimate_name) |>
    dplyr::mutate(row = dplyr::row_number()) |>
    tidyr::pivot_wider(names_from = "variable_name",
                       values_from = "variable_level") |>
    tidyr::pivot_wider(names_from = "estimate_name",
                       values_from = "estimate_value") |>
    dplyr::select(-"row") |>
    dplyr::ungroup() |>
    dplyr::rename("time" = "temporal_symmetry") |>
    dplyr::filter(.data$time != 0) |>
    dplyr::mutate(colour = dplyr::if_else(.data$time > 0, "B", "A")) |>
    dplyr::mutate(index_name = paste0("index = ", .data$index_name),
                  marker_name = paste0("marker = ", .data$marker_name)) |>
    dplyr::mutate(count = as.integer(.data$count),
                  time = as.integer(.data$time)) |>
    dplyr::compute()

  colours = c("A" = colours[1], "B" = colours[2])

  width_range <- (xlim[2] - xlim[1])/2

  ggplot2::ggplot(data = plot_data, ggplot2::aes(
    x = .data$time, y = .data$count, fill = .data$colour)) +
    ggplot2::geom_col(width = 0.01*width_range) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$colour), size = 1) +
    ggplot2::coord_cartesian(xlim = c(xlim[1], xlim[2])) +
    ggplot2::labs(title = plotTitle, x = labs[1], y = labs[2]) +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::facet_wrap(~ index_name + marker_name, scales = scales) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::scale_colour_manual(values = colours)
}
