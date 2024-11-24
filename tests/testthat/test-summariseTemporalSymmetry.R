test_that("test summariseTemporalSymmetry", {
  cdm <- mockCohortSymmetry()
  cdm <- generateSequenceCohortSet(
    cdm = cdm,
    name = "joined_cohorts",
    indexTable = "cohort_1",
    markerTable = "cohort_2"
  )
  temporal_symmetry <-
    summariseTemporalSymmetry(cohort = cdm$joined_cohorts)

  expect_true(all(
    names(temporal_symmetry) %in% c(
      "result_id",
      "cdm_name",
      "group_name",
      "group_level",
      "strata_name",
      "strata_level",
      "variable_name",
      "variable_level",
      "estimate_name",
      "estimate_type",
      "estimate_value",
      "additional_name",
      "additional_level"
    )
  ))

  temporal_symmetry <-
    summariseTemporalSymmetry(cohort = cdm$joined_cohorts, minCellCount = 0)

  expect_true(all(!is.na(
    temporal_symmetry$estimate_value |> unique()
  )))

  time <-
    cdm$joined_cohorts %>% dplyr::filter(cohort_definition_id == 1) %>%
    dplyr::mutate(time = as.numeric(
      !!CDMConnector::datediff("index_date", "marker_date", interval = "month")
    )) |> dplyr::pull(time)

  time2 <-
    temporal_symmetry %>% dplyr::filter(group_level == "cohort_1 &&& cohort_1") |> dplyr::pull(variable_level) |> as.double()

  expect_true(all(sum(time) == sum(time2)))


  temporal_symmetry <-
    summariseTemporalSymmetry(
      cohort = cdm$joined_cohorts,
      minCellCount = 0,
      timescale = "day"
    )

  time <-
    cdm$joined_cohorts %>% dplyr::filter(cohort_definition_id == 1) %>%
    dplyr::mutate(time = as.numeric(
      !!CDMConnector::datediff("index_date", "marker_date", interval = "day")
    )) |> dplyr::pull(time)

  time2 <-
    temporal_symmetry %>% dplyr::filter(group_level == "cohort_1 &&& cohort_1") |> dplyr::pull(variable_level) |> as.double()

  expect_true(all(sum(time) == sum(time2)))

  CDMConnector::cdmDisconnect(cdm = cdm)
})

test_that("test cohortId",{
  skip_on_cran()
  cdm <- mockCohortSymmetry()
  cdm <- generateSequenceCohortSet(
    cdm = cdm,
    name = "joined_cohorts",
    indexTable = "cohort_1",
    markerTable = "cohort_2"
  )

    result <- summariseTemporalSymmetry(cohort = cdm$joined_cohorts,
                                        cohortId = 1)

  expect_equal(omopgenerics::settings(result) %>%
                 nrow()|>
                 as.numeric(), 1)

  expect_true((result %>%
                 dplyr::select(group_level) %>%
                 dplyr::distinct(group_level) %>%
                 dplyr::pull(group_level)) == "cohort_1 &&& cohort_1")

  CDMConnector::cdm_disconnect(cdm = cdm)
})

test_that("input validation",{
  skip_on_cran()
  cdm <- mockCohortSymmetry()
  cdm <- generateSequenceCohortSet(
    cdm = cdm,
    name = "joined_cohorts",
    indexTable = "cohort_1",
    markerTable = "cohort_2"
  )

  expect_no_error(
    summariseTemporalSymmetry(cohort = cdm$joined_cohorts)
  )

  expect_no_error(
    summariseTemporalSymmetry(cohort = cdm$joined_cohorts,
                              cohortId = 1)
  )

  expect_error(
   summariseTemporalSymmetry(cohort = cdm$joined_cohorts,
                             timescale = "quarter")
  )

  expect_no_error(
    summariseTemporalSymmetry(cohort = cdm$joined_cohorts,
                              timescale = "day")
  )

  expect_no_error(
    summariseTemporalSymmetry(cohort = cdm$joined_cohorts,
                              timescale = "year")
  )

  expect_error(
    summariseTemporalSymmetry(cohort = cdm$joined_cohorts,
                              cohortId = "one")
  )

  expect_error(
    summariseTemporalSymmetry(cohort = cdm$joined_cohorts,
                              cohortId = "1")
  )

  expect_error(
    summariseTemporalSymmetry(cohort = cdm$joined_cohorts,
                              minCellCount = -1)
  )

  expect_error(
    summariseTemporalSymmetry(cohort = cdm$joined_cohorts,
                              minCellCount = Inf)
  )

  CDMConnector::cdmDisconnect(cdm = cdm)

})

test_that("edge case",{
  skip_on_cran()
  cdm <- mockCohortSymmetry()
  cdm <- generateSequenceCohortSet(
    cdm = cdm,
    name = "joined_cohorts",
    indexTable = "cohort_1",
    markerTable = "cohort_2",
    combinationWindow = c(10, 11)
  )

  expect_error(
    result <- summariseTemporalSymmetry(cohort = cdm$joined_cohorts)
  )

})
