test_that("summariseSequenceRatios", {
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
    expect_no_error(
      res <- summariseSequenceRatios(
        cohort = cdm$joined_cohorts)
    )
  )

  expect_true("summarised_result" %in% class(res))
  expect_error(
    summariseSequenceRatios(
      cohort = cdm$joined_cohorts2)
  )
  expect_error(
    summariseSequenceRatios(
      cohort = cdm$joined_cohorts,
      confidenceInterval = 101)
  )

  expect_error(
    summariseSequenceRatios(
      cohort = cdm$joined_cohorts,
      confidenceInterval = -101)
  )
    CDMConnector::cdm_disconnect(cdm = cdm)
})

test_that("summariseSequenceRatios - testing ratios and CIs, Example 1", {
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

  suppressWarnings(
    res <- summariseSequenceRatios(
      cohort = cdm$joined_cohorts,
      minCellCount = 0)
  )

  res <- res |>
    visOmopResults::splitAll() |>
    dplyr::filter(variable_name != "settings") |>
    dplyr::select(-"estimate_type") |>
    tidyr::pivot_wider(names_from = c("variable_level", "variable_name", "estimate_name"),
                       values_from = "estimate_value") |>
    dplyr::left_join(res |> omopgenerics::settings())

  expect_true(all(res$days_prior_observation==0))
  expect_true(all(res$washout_window==0))
  expect_true(all(res$combination_window == "(0,365)"))
  expect_true(all(res$index_marker_gap=="Inf"))
  expect_true(all(res$confidence_interval==95))
  expect_true(all(as.integer(res$first_pharmac_index_percentage)<=100 & 0 <= as.integer(res$first_pharmac_index_percentage)))

  int <- res %>%
    dplyr::mutate(crude_ci_check = .data$sequence_ratio_crude_lower_CI <= .data$sequence_ratio_crude_upper_CI)

  expect_true(all(as.integer(int$crude_ci_check== T)))
  CDMConnector::cdmDisconnect(cdm)
  })

test_that("summariseSequenceRatios - testing ratios and CIs, Example 2", {
  indexCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    subject_id = c(1, 4, 2, 3, 5, 7, 8, 9, 6, 10),
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
    cohort_definition_id = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3),
    subject_id = c(1, 3, 4, 2, 5, 10, 9, 8, 7, 11, 6),
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

  res <- summariseSequenceRatios(
    cohort = cdm$joined_cohorts,
    minCellCount = 0)

  res <- res |>
    visOmopResults::splitAll() |>
    dplyr::filter(variable_name != "settings") |>
    dplyr::select(-"estimate_type") |>
    tidyr::pivot_wider(names_from = c("variable_level", "variable_name", "estimate_name"),
                       values_from = "estimate_value") |>
    dplyr::left_join(res |> omopgenerics::settings())

  expect_true(all(res$days_prior_observation==0))
  expect_true(all(res$washout_window==0))
  expect_true(all(res$combination_window == "(0,365)"))
  expect_true(all(res$index_marker_gap=="Inf"))
  expect_true(all(res$confidence_interval==95))
  expect_true((res$index_cohort_name=="cohort_1"))
  expect_true((res$marker_cohort_name=="cohort_3"))
  expect_true(all(as.integer(res$first_pharmac_index_percentage)<=100 & 0 <= as.integer(res$first_pharmac_index_percentage)))

  int <- res %>%
    dplyr::mutate(crude_ci_check = .data$sequence_ratio_crude_lower_CI <= .data$sequence_ratio_crude_upper_CI,
                  adjusted_ci_check = .data$sequence_ratio_adjusted_lower_CI <= .data$sequence_ratio_adjusted_upper_CI)

  expect_true(all(as.integer(int$crude_ci_check== T)))
  expect_true(all(as.integer(int$adjusted_ci_check== T)))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("summariseSequenceRatios - testing CI", {
  skip_on_cran()
  indexCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    subject_id = c(1, 4, 2, 3, 5, 7, 8, 9, 6, 10),
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
    cohort_definition_id = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3),
    subject_id = c(1, 3, 4, 2, 5, 10, 9, 8, 7, 11, 6),
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

  res_90 <- summariseSequenceRatios(
    cohort = cdm$joined_cohorts,
    confidenceInterval = 90,
    minCellCount = 0)

  expect_equal(res_90 %>% dplyr::filter(variable_level == "first_pharmac",
                                        variable_name == "index",
                                        estimate_name == "count") %>%
                 dplyr::pull("estimate_value") %>%
                 as.numeric(),
               3)

  expect_equal(res_90 %>% dplyr::filter(variable_level == "first_pharmac",
                                        variable_name == "index",
                                        estimate_name == "percentage") %>%
                 dplyr::pull("estimate_value") %>%
                 as.numeric(),
               75)
  expect_equal(res_90 %>% dplyr::filter(variable_level == "first_pharmac",
                                        variable_name == "marker",
                                        estimate_name == "count") %>%
                 dplyr::pull("estimate_value") %>%
                 as.numeric(),
               1)

  expect_equal(res_90 %>% dplyr::filter(variable_level == "first_pharmac",
                                        variable_name == "marker",
                                        estimate_name == "percentage") %>%
                 dplyr::pull("estimate_value") %>%
                 as.numeric(),
               25)

  expect_equal(res_90 %>% dplyr::filter(variable_level == "sequence_ratio",
                                        variable_name == "crude",
                                        estimate_name == "point_estimate") %>%
                 dplyr::pull("estimate_value") %>%
                 as.numeric(),
               3)

  res_95 <- summariseSequenceRatios(
    cohort = cdm$joined_cohorts,
    confidenceInterval = 95,
    minCellCount = 0)

  expect_equal(res_95 %>% dplyr::filter(variable_level == "first_pharmac",
                                        variable_name == "index",
                                        estimate_name == "count") %>%
                 dplyr::pull("estimate_value") %>%
                 as.numeric(),
               3)

  expect_equal(res_95 %>% dplyr::filter(variable_level == "first_pharmac",
                                        variable_name == "index",
                                        estimate_name == "percentage") %>%
                 dplyr::pull("estimate_value") %>%
                 as.numeric(),
               75)
  expect_equal(res_95 %>% dplyr::filter(variable_level == "first_pharmac",
                                        variable_name == "marker",
                                        estimate_name == "count") %>%
                 dplyr::pull("estimate_value") %>%
                 as.numeric(),
               1)

  expect_equal(res_95 %>% dplyr::filter(variable_level == "first_pharmac",
                                        variable_name == "marker",
                                        estimate_name == "percentage") %>%
                 dplyr::pull("estimate_value") %>%
                 as.numeric(),
               25)

  expect_equal(res_95 %>% dplyr::filter(variable_level == "sequence_ratio",
                                        variable_name == "crude",
                                        estimate_name == "point_estimate") %>%
                 dplyr::pull("estimate_value") %>%
                 as.numeric(),
               3)

  res_99 <- summariseSequenceRatios(
    cohort = cdm$joined_cohorts,
    confidenceInterval = 99,
    minCellCount = 0)

  expect_equal(res_99 %>% dplyr::filter(variable_level == "first_pharmac",
                                        variable_name == "index",
                                        estimate_name == "count") %>%
                 dplyr::pull("estimate_value") %>%
                 as.numeric(),
               3)

  expect_equal(res_99 %>% dplyr::filter(variable_level == "first_pharmac",
                                        variable_name == "index",
                                        estimate_name == "percentage") %>%
                 dplyr::pull("estimate_value") %>%
                 as.numeric(),
               75)
  expect_equal(res_99 %>% dplyr::filter(variable_level == "first_pharmac",
                                        variable_name == "marker",
                                        estimate_name == "count") %>%
                 dplyr::pull("estimate_value") %>%
                 as.numeric(),
               1)

  expect_equal(res_99 %>% dplyr::filter(variable_level == "first_pharmac",
                                        variable_name == "marker",
                                        estimate_name == "percentage") %>%
                 dplyr::pull("estimate_value") %>%
                 as.numeric(),
               25)

  expect_equal(res_99 %>% dplyr::filter(variable_level == "sequence_ratio",
                                        variable_name == "crude",
                                        estimate_name == "point_estimate") %>%
                 dplyr::pull("estimate_value") %>%
                 as.numeric(),
               3)

  expect_true(
    (res_90 %>%
      dplyr::filter(estimate_name == "lower_CI",
                    variable_name == "crude") %>%
      dplyr::pull(estimate_value) %>%
      as.numeric()) >=
    (res_95 %>%
       dplyr::filter(estimate_name == "lower_CI",
                     variable_name == "crude") %>%
       dplyr::pull(estimate_value) %>%
       as.numeric())
  )

  expect_true(
    (res_95 %>%
       dplyr::filter(estimate_name == "lower_CI",
                     variable_name == "crude") %>%
       dplyr::pull(estimate_value) %>%
       as.numeric()) >=
      (res_99 %>%
         dplyr::filter(estimate_name == "lower_CI",
                       variable_name == "crude") %>%
         dplyr::pull(estimate_value) %>%
         as.numeric())
  )

  expect_true(
    (res_90 %>%
       dplyr::filter(estimate_name == "upper_CI",
                     variable_name == "crude") %>%
       dplyr::pull(estimate_value) %>%
       as.numeric()) <=
      (res_95 %>%
         dplyr::filter(estimate_name == "upper_CI",
                       variable_name == "crude") %>%
         dplyr::pull(estimate_value) %>%
         as.numeric())
  )

  expect_true(
    (res_95 %>%
       dplyr::filter(estimate_name == "upper_CI",
                     variable_name == "crude") %>%
       dplyr::pull(estimate_value) %>%
       as.numeric()) <=
      (res_99 %>%
         dplyr::filter(estimate_name == "upper_CI",
                       variable_name == "crude") %>%
         dplyr::pull(estimate_value) %>%
         as.numeric())
  )

  CDMConnector::cdmDisconnect(cdm)
})

test_that("summariseSequenceRatios - testing cohortId", {
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
                                   markerTable = "cohort_2",
                                   daysPriorObservation = 0,
                                   combinationWindow = c(0, Inf))

  expect_no_error(
    result <- summariseSequenceRatios(cohort = cdm$joined_cohorts,
                                     cohortId = 1,
                                     minCellCount = 0) %>%
      visOmopResults::splitAll()
  )

  expect_equal(
    result %>%
      dplyr::select("index_cohort_name") %>%
      dplyr::distinct() %>%
      as.character(),
    "cohort_1"
  )

  expect_equal(
    result %>%
      dplyr::select("marker_cohort_name") %>%
      dplyr::distinct() %>%
      as.character(),
    "cohort_1"
  )

  expect_equal(
    attr(cdm$joined_cohorts, "cohort_set") %>%
      dplyr::collect() %>%
      dplyr::filter(cohort_definition_id == 1) |>
      nrow() |>
      as.numeric(),
    1
  )

  expect_equal(
    attr(cdm$joined_cohorts, "cohort_set") %>%
      dplyr::collect() %>%
      dplyr::filter(cohort_definition_id == 1) |>
      dplyr::pull("index_name"),
    "cohort_1")

  expect_equal(
    attr(cdm$joined_cohorts, "cohort_set") %>%
      dplyr::collect() %>%
      dplyr::filter(cohort_definition_id == 1) |>
      dplyr::pull("marker_name"),
    "cohort_1")

  expect_no_error(
    result2 <- summariseSequenceRatios(cohort = cdm$joined_cohorts,
                                     cohortId = c(1,3),
                                     minCellCount = 0) %>%
      visOmopResults::splitAll()
  )

  expect_equal(
    result2 %>%
      dplyr::group_by(index_cohort_name, marker_cohort_name) %>%
      dplyr::tally() |>
      nrow() |>
      as.numeric(),
    2
  )

  CDMConnector::cdmDisconnect(cdm)
})

test_that("summariseSequenceRatios - testing moving average restriction, ex1", {
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

  expect_no_error(
    cdm <- generateSequenceCohortSet(cdm = cdm,
                                     name = "joined_cohorts",
                                     indexTable = "cohort_1",
                                     markerTable = "cohort_2",
                                     daysPriorObservation = 0,
                                     combinationWindow = c(0, Inf),
                                     movingAverageRestriction = 730)
  )

  expect_no_error(
    result <- summariseSequenceRatios(cohort = cdm$joined_cohorts,
                                      cohortId = 1,
                                      minCellCount = 0)
  )

  expect_true(
    (result %>%
      dplyr::filter(variable_name == "crude", estimate_name == "lower_CI") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric())

    <=

      (result %>%
         dplyr::filter(variable_name == "crude", estimate_name == "upper_CI") %>%
         dplyr::pull("estimate_value") %>%
         as.numeric())
  )

  expect_true(
    (result %>%
       dplyr::filter(variable_name == "adjusted", estimate_name == "lower_CI") %>%
       dplyr::pull("estimate_value") %>%
       as.numeric())

    <=

      (result %>%
         dplyr::filter(variable_name == "adjusted", estimate_name == "upper_CI") %>%
         dplyr::pull("estimate_value") %>%
         as.numeric())
  )

  CDMConnector::cdmDisconnect(cdm)
})

test_that("summariseSequenceRatios - testing moving average restriction, ex2", {
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

  expect_no_error(
    cdm <- generateSequenceCohortSet(cdm = cdm,
                                     name = "joined_cohorts",
                                     indexTable = "cohort_1",
                                     markerTable = "cohort_2",
                                     daysPriorObservation = 0,
                                     combinationWindow = c(0, Inf),
                                     movingAverageRestriction = Inf)
  )

  expect_no_error(
    result <- summariseSequenceRatios(cohort = cdm$joined_cohorts,
                                      minCellCount = 0)
  )

  expect_true(all(
    (result %>%
       dplyr::filter(variable_name == "crude", estimate_name == "lower_CI") %>%
       dplyr::pull("estimate_value") %>%
       as.numeric())

    <=

      (result %>%
         dplyr::filter(variable_name == "crude", estimate_name == "upper_CI") %>%
         dplyr::pull("estimate_value") %>%
         as.numeric())
  ))

  expect_true(all(
    (result %>%
       dplyr::filter(variable_name == "adjusted", estimate_name == "lower_CI") %>%
       dplyr::pull("estimate_value") %>%
       as.numeric())

    <=

      (result %>%
         dplyr::filter(variable_name == "adjusted", estimate_name == "upper_CI") %>%
         dplyr::pull("estimate_value") %>%
         as.numeric())
  ))

  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   name = "joined_cohorts",
                                   indexTable = "cohort_1",
                                   markerTable = "cohort_2",
                                   daysPriorObservation = 0,
                                   combinationWindow = c(0, Inf),
                                   movingAverageRestriction = Inf)

  res_90 <- summariseSequenceRatios(
    cohort = cdm$joined_cohorts,
    cohortId = 1,
    confidenceInterval = 90,
    minCellCount = 0)

  res_95 <- summariseSequenceRatios(
    cohort = cdm$joined_cohorts,
    cohortId = 1,
    confidenceInterval = 95,
    minCellCount = 0)

  res_99 <- summariseSequenceRatios(
    cohort = cdm$joined_cohorts,
    cohortId = 1,
    confidenceInterval = 99,
    minCellCount = 0)

  expect_true(
    (res_90 %>%
       dplyr::filter(estimate_name == "lower_CI",
                     variable_name == "crude") %>%
       dplyr::pull(estimate_value) %>%
       as.numeric()) >=
      (res_95 %>%
         dplyr::filter(estimate_name == "lower_CI",
                       variable_name == "crude") %>%
         dplyr::pull(estimate_value) %>%
         as.numeric())
  )

  expect_true(
    (res_95 %>%
       dplyr::filter(estimate_name == "lower_CI",
                     variable_name == "crude") %>%
       dplyr::pull(estimate_value) %>%
       as.numeric()) >=
      (res_99 %>%
         dplyr::filter(estimate_name == "lower_CI",
                       variable_name == "crude") %>%
         dplyr::pull(estimate_value) %>%
         as.numeric())
  )

  expect_true(
    (res_90 %>%
       dplyr::filter(estimate_name == "upper_CI",
                     variable_name == "crude") %>%
       dplyr::pull(estimate_value) %>%
       as.numeric()) <=
      (res_95 %>%
         dplyr::filter(estimate_name == "upper_CI",
                       variable_name == "crude") %>%
         dplyr::pull(estimate_value) %>%
         as.numeric())
  )

  expect_true(
    (res_95 %>%
       dplyr::filter(estimate_name == "upper_CI",
                     variable_name == "crude") %>%
       dplyr::pull(estimate_value) %>%
       as.numeric()) <=
      (res_99 %>%
         dplyr::filter(estimate_name == "upper_CI",
                       variable_name == "crude") %>%
         dplyr::pull(estimate_value) %>%
         as.numeric())
  )

  CDMConnector::cdmDisconnect(cdm)
})

test_that("edge case 1", {
  skip_on_cran()
  indexCohort <- dplyr::tibble(
    cohort_definition_id = c(1),
    subject_id = c(1),
    cohort_start_date = as.Date(
      c("2020-04-01")
    ),
    cohort_end_date = as.Date(
      c("2020-04-01")
    )
  )

  markerCohort <- dplyr::tibble(
    cohort_definition_id = c(1),
    subject_id = c(1),
    cohort_start_date = as.Date(
      c("2020-04-01")
    ),
    cohort_end_date = cohort_start_date
  )

  cdm <- mockCohortSymmetry(indexCohort = indexCohort,
                            markerCohort = markerCohort)

  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   name = "joined_cohorts",
                                   indexTable = "cohort_1",
                                   markerTable = "cohort_2",
                                   daysPriorObservation = 0,
                                   combinationWindow = c(0, Inf))

  expect_error(
    result <- summariseSequenceRatios(cohort = cdm$joined_cohorts)
  )
  CDMConnector::cdm_disconnect(cdm = cdm)
})

test_that("edge case 2", {
  skip_on_cran()
  indexCohort <- dplyr::tibble(
    cohort_definition_id = c(1),
    subject_id = c(1),
    cohort_start_date = as.Date(
      c("2020-04-01")
    ),
    cohort_end_date = as.Date(
      c("2020-04-01")
    )
  )

  markerCohort <- dplyr::tibble(
    cohort_definition_id = c(1),
    subject_id = c(1),
    cohort_start_date = as.Date(
      c("2020-04-02")
    ),
    cohort_end_date = cohort_start_date
  )

  cdm <- mockCohortSymmetry(indexCohort = indexCohort,
                            markerCohort = markerCohort)

  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   name = "joined_cohorts",
                                   indexTable = "cohort_1",
                                   markerTable = "cohort_2",
                                   daysPriorObservation = 0,
                                   combinationWindow = c(0, Inf))

  expect_warning(
    result <- summariseSequenceRatios(cohort = cdm$joined_cohorts)
  )
  CDMConnector::cdm_disconnect(cdm = cdm)
})

test_that("edge case 3", {
  skip_on_cran()
  indexCohort <- dplyr::tibble(
    cohort_definition_id = c(1),
    subject_id = c(1),
    cohort_start_date = as.Date(
      c("2020-04-03")
    ),
    cohort_end_date = as.Date(
      c("2020-04-03")
    )
  )

  markerCohort <- dplyr::tibble(
    cohort_definition_id = c(1),
    subject_id = c(1),
    cohort_start_date = as.Date(
      c("2020-04-02")
    ),
    cohort_end_date = cohort_start_date
  )

  cdm <- mockCohortSymmetry(indexCohort = indexCohort,
                            markerCohort = markerCohort)

  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   name = "joined_cohorts",
                                   indexTable = "cohort_1",
                                   markerTable = "cohort_2",
                                   daysPriorObservation = 0,
                                   combinationWindow = c(0, Inf))

  expect_warning(
    result <- summariseSequenceRatios(cohort = cdm$joined_cohorts)
  )
  CDMConnector::cdm_disconnect(cdm = cdm)
})

test_that("min cell count",{
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

  expect_no_error(
    expect_warning(
    result1 <- summariseSequenceRatios(
      cohort = cdm$joined_cohorts,
      minCellCount = 0
    )
  )
  )

  expect_error(
    result_error <- summariseSequenceRatios(
      cohort = cdm$joined_cohort,
      minCellCount = Inf
    )
  )

  expect_error(
    result_error2 <- summariseSequenceRatios(
      cohort = cdm$joined_cohort,
      minCellCount = -1
    )
  )

  expect_no_error(
    expect_warning(
    result2 <- summariseSequenceRatios(
      cohort = cdm$joined_cohorts,
      minCellCount = 10
    )
  )
  )

  expect_equal(
    result1 %>%
      nrow()|>
      as.numeric(),
    result2 %>%
      nrow()|>
      as.numeric())

  CDMConnector::cdm_disconnect(cdm = cdm)
})
