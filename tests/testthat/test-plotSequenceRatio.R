test_that("plot working", {
  cdm <- omock::mockCdmReference(cdmName = "mock") |>
    omock::mockPerson(nPerson = 100) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(
      name = "marker_cohort",
      numberCohorts = 2,
      cohortName = c("marker_a", "marker_b"),
      seed = 11
    ) |>
    omock::mockCohort(
      name = "index_cohort",
      numberCohorts = 2,
      cohortName = c("index_a", "index_b"),
      seed = 33
    )
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cdm <- CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)
  cdm <- generateSequenceCohortSet(cdm, "index_cohort", "marker_cohort", "joined_cohort", combinationWindow = c(0, Inf))
  result <- summariseSequenceRatios(cohort = cdm$joined_cohort)

  plotSR1 <- plotSequenceRatios(result)
  plotSR2 <- plotSequenceRatios(result, plotTitle = "Test plot")
  plotSR3 <- plotSequenceRatios(result, labs = c("xlab", "ylab"))
  plotSR4 <- plotSequenceRatios(result, colours = c("blue", "green"))
  plotSR5 <- plotSequenceRatios(result, onlyASR = TRUE, colours = c("orange"))

  expect_true("ggplot" %in% (plotSR1 %>% class()))
  expect_true("ggplot" %in% (plotSR2 %>% class()))
  expect_true("ggplot" %in% (plotSR3 %>% class()))
  expect_true("ggplot" %in% (plotSR4 %>% class()))
  expect_true("ggplot" %in% (plotSR5 %>% class()))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("expected errors", {
  skip_on_cran()
  cdm <- omock::mockCdmReference(cdmName = "mock") |>
    omock::mockPerson(nPerson = 100) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(
      name = "marker_cohort",
      numberCohorts = 2,
      cohortName = c("marker_a", "marker_b"),
      seed = 11
    ) |>
    omock::mockCohort(
      name = "index_cohort",
      numberCohorts = 2,
      cohortName = c("index_a", "index_b"),
      seed = 33
    )
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cdm <- CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)
  cdm <- generateSequenceCohortSet(cdm, "index_cohort", "marker_cohort", "joined_cohort", combinationWindow = c(0, Inf))
  result <- summariseSequenceRatios(cohort = cdm$joined_cohort)
  result2 <- result %>%
    dplyr::select(-c("group_level"))

  expect_error(plotSequenceRatios("result"))
  expect_error(plotSequenceRatios("result22"))
  expect_error(plotSequenceRatios(result2))
  expect_error(plotSequenceRatios(result, colours = "red"))
  expect_error(plotSequenceRatios(result, colours = c(3,4)))
  expect_error(plotSequenceRatios(result, onlyASR = 3))
  expect_error(plotSequenceRatios(result, onlyASR = TRUE, colours = c("red", "blue")))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("empty result error",{
  skip_on_cran()
  indexCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
    subject_id = c(1, 4, 2, 3, 5, 5, 4, 3, 6, 1),
    cohort_start_date = as.Date(
      c(
        "2020-04-01", "2021-06-01", "2022-05-22", "2010-01-01", "2019-08-01", "2019-04-07", "2021-01-01", "2008-02-02", "2010-09-09", "2021-01-01"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-04-01", "2021-08-01", "2022-05-23", "2010-03-01", "2020-04-01", "2020-05-30", "2022-02-02", "2013-12-03", "2010-11-01", "2021-01-01"
      )
    )
  )|>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  markerCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3),
    subject_id = c(1, 3, 4, 2, 5, 1, 2, 3, 4, 5, 6),
    cohort_start_date = as.Date(
      c(
        "2020-12-30", "2010-01-01","2021-05-25","2022-05-31", "2020-05-25", "2019-05-25", "2022-05-25", "2010-09-30", "2022-05-25", "2020-02-29", "2021-01-01"
      )
    ),
    cohort_end_date = cohort_start_date
  )|>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  cdm <- mockCohortSymmetry(indexCohort = indexCohort,
                            markerCohort = markerCohort)

  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   name = "joined_cohorts",
                                   indexTable = "cohort_1",
                                   markerTable = "cohort_2")

  expect_warning(
    sr <- summariseSequenceRatios(cohort = cdm$joined_cohorts)
  )

  expect_warning(
  sr2 <- summariseSequenceRatios(cohort = cdm$joined_cohorts,
                                 minCellCount = 0)
  )

  expect_no_error(
    plotSequenceRatios(sr2)
  )

  CDMConnector::cdm_disconnect(cdm = cdm)
})
