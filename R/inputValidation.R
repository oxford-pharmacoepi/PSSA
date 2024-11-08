checkInputGenerateSequenceCohortSet <- function(cdm,
                                        indexTable,
                                        markerTable,
                                        name,
                                        cohortDateRange,
                                        indexId,
                                        markerId,
                                        daysPriorObservation,
                                        washoutWindow,
                                        indexMarkerGap,
                                        combinationWindow,
                                        movingAverageRestriction
                                        ) {

  # Check cdm objects, writing schema and index/marker tables
  checkCdm(cdm, tables = c(indexTable, markerTable))

  # Check the format of name
  if(stringr::str_detect(name, "^[a-z0-9_]+$", negate = TRUE)){
    cli::cli_abort(c("name must be given in snake case",
                     "i" = "for example 'my_cohort' is allowed but 'MyCohort' is not"))
  }

  # Check the rest of inputs
  errorMessage <- checkmate::makeAssertCollection()

  ## check name format
  checkmate::assertCharacter(name, len = 1, any.missing = FALSE, add = errorMessage)

  ## Check date
  checkCohortDateRange(cohortDateRange, errorMessage)

  ## Checks that Index and Marker ids exist in Index and Marker tables
  checkCohortIds(cohort = cdm[[indexTable]],
                 cohortId = indexId,
                 errorMessage = errorMessage)
  checkCohortIds(cohort = cdm[[markerTable]],
                 cohortId = markerId,
                 errorMessage = errorMessage)

  ## Checks columns in Index and Marker tables
  checkColumns(cdm, indexTable, errorMessage)
  checkColumns(cdm, markerTable, errorMessage)

  ## Check daysPriorObservation
  checkDaysPriorObservation(daysPriorObservation, errorMessage)

  ## Check combinationWindow
  checkCombinationWindow(combinationWindow, errorMessage)

  ## Check indexMarkerGap
  checkIndexMarkerGap(indexMarkerGap, combinationWindow, errorMessage)

  ## Check washoutWindow
  checkWashoutWindow(washoutWindow, errorMessage)

  ## Check movingAverageRestriction
  checkMovingAverageRestriction(movingAverageRestriction, errorMessage)

  # Report errors
  checkmate::reportAssertions(collection = errorMessage)
}

checkInputSummariseSequenceRatios <- function(cohort,
                                              cohortId,
                                              confidenceInterval,
                                              minCellCount) {

  # Check cdm objects, writing schema and index/marker tables
  cdm <- omopgenerics::cdmReference(cohort)
  checkCdm(cdm)

  cohort_row <- cohort |> dplyr::tally() |> dplyr::pull()
  if (cohort_row <=0){
    cli::cli_abort("Aborted! The cohort has no rows, please revisit the cohort")
  }

  checkCohortIds(cohort = cohort,
                 cohortId = cohortId,
                 errorMessage = errorMessage)

  # Check the rest of inputs
  errorMessage <- checkmate::makeAssertCollection()

  # Check minCellCount
  checkMinCellCount(minCellCount, errorMessage)

  ## Check confidenceInterval
  checkConfidenceInterval(confidenceInterval, errorMessage)

  # Report errors
  checkmate::reportAssertions(collection = errorMessage)
}

checkInputSummariseTemporalSymmetry <- function(cohort,
                                                cohortId,
                                                timescale,
                                                minCellCount) {

  # Check cdm objects, writing schema and index/marker tables
  cdm <- omopgenerics::cdmReference(cohort)
  checkCdm(cdm)

  cohort_row <- cohort |> dplyr::tally() |> dplyr::pull()
  if (cohort_row <=0){
    cli::cli_abort("Aborted! The cohort has no rows, please revisit the cohort")
  }

  checkCohortIds(cohort = cohort,
                 cohortId = cohortId,
                 errorMessage = errorMessage)

  # Check the rest of inputs
  errorMessage <- checkmate::makeAssertCollection()

  # Check minCellCount
  checkMinCellCount(minCellCount, errorMessage)

  # Check timescale
  checkTimeScale(timescale, errorMessage)

  # Report errors
  checkmate::reportAssertions(collection = errorMessage)
}

checkSequenceSymmetry <- function(result) {
  omopgenerics::newSummarisedResult(result)
}

checksFormatSequenceSymmetry <- function(type, crude, adjusted, studyPopulation,
                                         indexName, markerName, cdmName, .options) {
  # Checks
  errorMessage <- checkmate::makeAssertCollection()
  ## Booleans
  for (boolean in c(crude, adjusted, studyPopulation, indexName, markerName, cdmName)) {
    checkSingleBoolean(boolean, errorMessage)
  }
  ## Type
  checkType(type, errorMessage)
  ## .options
  checkOptions(.options, errorMessage)
  # Report errors
  checkmate::reportAssertions(collection = errorMessage)
}

checkInputPlotTemporalSymmetry <- function(result,
                                           plotTitle,
                                           labs,
                                           xlim,
                                           colours,
                                           scales) {

  result_check <- result |>
    dplyr::pull("estimate_value")

  if (all(is.na(result_check))){
    cli::cli_abort("Aborted! All the temporal symmetry results are NAs, no plots
    could be produced")
  }

  # Check the rest of inputs
  errorMessage <- checkmate::makeAssertCollection()

  ## Check result
  checkSequenceSymmetry(result)

  ## Check plot title and labs
  checkPlotTitleLabs(plotTitle, labs, errorMessage)

  ## Check xlim
  checkXLim(xlim, errorMessage)

  ## Check colours
  checkColours(colours, errorMessage)

  ## Check scales
  checkScales(scales, errorMessage)

  # Report errors
  checkmate::reportAssertions(collection = errorMessage)
}

checkInputPlotSequenceRatios <- function(result,
                                        onlyaSR,
                                        plotTitle,
                                        labs,
                                        colours) {

  result_check <- result |>
    dplyr::filter(.data$estimate_name == "point_estimate") |>
    dplyr::pull("estimate_value")

  if (all(is.na(result_check))){
    cli::cli_abort("Aborted! All the sequence ratios are NAs, no plots could be
                   produced")
  }

  # Check the rest of inputs
  errorMessage <- checkmate::makeAssertCollection()

  ## Check result
  checkSequenceSymmetry(result)

  ## Check plot title and labs
  checkPlotTitleLabs(plotTitle, labs, errorMessage)

  ## Check colours and onlyaSR
  checkColoursaSR(colours, onlyaSR, errorMessage)

  # Report errors
  checkmate::reportAssertions(collection = errorMessage)
}

####################################################################
# Check cdm object and index/marker tables
checkCdm <- function(cdm, tables = NULL) {
  if (!isTRUE(inherits(cdm, "cdm_reference"))) {
    cli::cli_abort("cdm must be a CDMConnector CDM reference object")
  }
  if (!is.null(tables)) {
    tables <- tables[!(tables %in% names(cdm))]
    if (length(tables) > 0) {
      cli::cli_abort(paste0(
        "tables: ", tables,
        " are not present in the cdm object"
      ))
    }
  }
  invisible(NULL)
}

# Checks Index and Marker ids cohorts
checkCohortIds <- function(cohort, cohortId, errorMessage) {
  checkmate::assertNumeric(cohortId, lower = 1, any.missing = FALSE,
                           null.ok = TRUE, add = errorMessage)
  if (!is.null(cohortId)) {
    ids <- cohort |>
      dplyr::select("cohort_definition_id") |>
      dplyr::distinct() |>
      dplyr::pull()
    if(!isTRUE(all(cohortId %in% ids))){
      errorMessage$push("Some of the cohort ids given do not exist in the cohort table(s) provided.")
    }
  }
}

# Checks columns of Index and Marker tables
checkColumns <- function(cdm, CohortTable, errorMessage) {
  col <- colnames(cdm[[CohortTable]])
  exp_col <- c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")
  if(!isTRUE(all(exp_col %in% col))){
    errorMessage$push(paste0("Some of the expected columns in ", CohortTable,
                             " are missing (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)."))
  }
}

# Check indexMarkerGap (Inf or numeric >=1)
checkIndexMarkerGap <- function(indexMarkerGap, combinationWindow, errorMessage) {
  if (!is.null(indexMarkerGap)) {
    if (indexMarkerGap != Inf) {
      checkmate::assertIntegerish(
        indexMarkerGap,
        lower = 0, any.missing = FALSE, max.len = 4, add = errorMessage
      )
    }
  }
}

# Check washoutWindow (Inf or numeric)
checkWashoutWindow <- function(washoutWindow, errorMessage) {
  if (washoutWindow != Inf) {
    checkmate::assertIntegerish(
      washoutWindow,
      lower = 0, any.missing = FALSE, max.len = 4, add = errorMessage
    )
  }
}

# Check movingAverageRestriction (Inf or numeric)
checkMovingAverageRestriction <- function(movingAverageRestriction, errorMessage){
  if (movingAverageRestriction != Inf) {
    checkmate::assertIntegerish(
      movingAverageRestriction,
      lower = 1, any.missing = FALSE, max.len = 10, add = errorMessage
    )
  }
}

# Check daysPriorObservation (has to be numeric)
checkDaysPriorObservation <- function(daysPriorObservation, errorMessage){
  if (daysPriorObservation != Inf) {
    checkmate::assertIntegerish(
      daysPriorObservation,
      lower = 0, any.missing = FALSE, max.len = 4, add = errorMessage
    )
  }
  if(!(is.finite(daysPriorObservation))){
    errorMessage$push("daysPriorObservation has to be finite.")
  }
}

# Check minCellCount (has to be numeric)
checkMinCellCount <- function(minCellCount, errorMessage){
  if (minCellCount != Inf) {
    checkmate::assertIntegerish(
      minCellCount,
      lower = 0, any.missing = FALSE, max.len = 4, add = errorMessage
    )
  }
  if(!(is.finite(minCellCount))){
    errorMessage$push("minCellCount has to be finite.")
  }
}

# Check combinationWindow (a numeric of length 2)
checkCombinationWindow <- function(combinationWindow, errorMessage){
  checkmate::assert_numeric(combinationWindow, len = 2, any.missing = FALSE, add = errorMessage)
  if (combinationWindow[1] == Inf) {
   errorMessage$push("The first argument of combinationWindow cannot be infinite.")
  }
  if (combinationWindow[2] != Inf){
    checkmate::assertIntegerish(
      combinationWindow[1],
      lower = 0, any.missing = FALSE, max.len = 4, add = errorMessage
    )
    checkmate::assertIntegerish(
      combinationWindow[2],
      lower = 1, any.missing = FALSE, max.len = 4, add = errorMessage
    )
  }
  if (combinationWindow[1] >= combinationWindow[2]) {
    errorMessage$push("The first argument of combinationWindow must be smaller than the second.")
  }
}

checkCohortDateRange <- function(cohortDateRange, errorMessage) {
  checkmate::assertDate(cohortDateRange, len = 2, add = errorMessage)
  if (all(!is.na(cohortDateRange))) {
    if (cohortDateRange[1] >= cohortDateRange[2]) {
      errorMessage$push("First element in cohortDateRange must be smaller than the second.")
    }
  }
}

checkConfidenceInterval <- function(confidenceInterval, errorMessage) {
  checkmate::assertNumeric(
    confidenceInterval, len = 1,
    lower = 0, upper = 100, any.missing = FALSE, add = errorMessage
  )
}

checkType <- function(type, errorMessage) {
  checkmate::assertCharacter(type, min.chars = 2, max.chars = 10, len = 1,
                             add = errorMessage)
}

checkSingleBoolean <- function(splitGroup, errorMessage) {
  checkmate::assertLogical(splitGroup, any.missing = FALSE, len = 1,
                           add = errorMessage)
}

checkOptions <- function(.options, errorMessage) {
  allowedNames <- names(tableSequenceRatiosOptions())
  optionsNames <- names(.options)
  checkmate::assertList(.options, null.ok = TRUE, any.missing = TRUE,
                        types = c("numeric", "logical", "character", "list"),
                        add = errorMessage)
  names_id <- optionsNames %in% allowedNames
  if(!all(names_id)) {
    errorMessage$push(
      paste0("The following elements are not supported arguments for .options: ",
             paste0(optionsNames[!names_id], collapse = ", "))
    )
  }
}

checkXLim <- function(xlim, errorMessage) {
  checkmate::assert_integerish(xlim,
                               len = 2,
                               add = errorMessage)
}

checkColours <- function(colours, errorMessage) {
  checkmate::assert_character(colours,
                                len = 2,
                                add = errorMessage)

  for(i in 1:length(colours)) {
    if(!(colours[i] %in% grDevices::colors())) {
      cli::cli_abort(message = paste0("colour '",colours[i],"' is not available. Please select one of the list of colours in base R, type colors()"))
    }
  }
}

checkColoursaSR <- function(colours, onlyaSR, errorMessage) {
  checkmate::assert_logical(onlyaSR,
                            add = errorMessage)
  if(onlyaSR) {
    checkmate::assert_character(colours,
                                len = 1,
                                add = errorMessage)
  } else {
    checkmate::assert_character(colours,
                                len = 2,
                                add = errorMessage)
  }

  for(i in 1:length(colours)) {
    if(!(colours[i] %in% grDevices::colors())) {
      cli::cli_abort(message = paste0("colour '",colours[i],"' is not available. Please select one of the list of colours in base R, type colors()"))
    }
  }
}

checkPlotTitleLabs <- function(plotTitle, labs, errorMessage) {
  checkmate::assert_character(plotTitle,
                              len = 1,
                              null.ok = TRUE,
                              add = errorMessage)

  checkmate::assert_character(labs,
                              len = 2,
                              add = errorMessage)
}

checkScales <- function(scales, errorMessage) {
  checkmate::assert_character(scales,
                              len = 1,
                              add = errorMessage)
  if(!(scales %in% c("free", "fixed"))) {
    cli::cli_abort("The parameter 'scales' can only be set to 'free' or 'fixed'.")
  }
}

checkTimeScale <- function(timescale, errorMessage){
  checkmate::assert_character(timescale,
                              len = 1,
                              add = errorMessage)
  if(!(timescale %in% c("day", "month", "year"))) {
    cli::cli_abort("The parameter 'timescale' can only be set to 'day', 'month' or 'year'.")
  }
}
