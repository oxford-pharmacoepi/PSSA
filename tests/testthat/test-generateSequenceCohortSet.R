#################################### Basic checks ####################################
test_that("check output table name", {
  skip_on_cran()
  cdm <- mockCohortSymmetry()
  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   name = "output",
                                   indexTable ="cohort_1",
                                   markerTable = "cohort_2")
  expect_true(all(
    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "marker_date",
      "index_date") %in%
      colnames(cdm$output)
  ))
  CDMConnector::cdmDisconnect(cdm)
}
)

# check colnames
test_that("check output format, colnames", {
  skip_on_cran()
  cdm <- mockCohortSymmetry()
  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   markerTable = "cohort_2")
  expect_true(all(
    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "marker_date",
      "index_date") %in%
      colnames(cdm$joined_cohorts)
  ))
  CDMConnector::cdmDisconnect(cdm)
}
)

test_that("one ID against one ID, example 1", {
  cdm <- mockCohortSymmetry()
  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   indexId=1,
                                   markerTable = "cohort_2",
                                   markerId=1)
  expect_true((cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n)) == 2)
  cdm$joined_cohorts <- cdm$joined_cohorts %>%
    dplyr::inner_join(omopgenerics::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::mutate(gap_between_index_marker = marker_date - index_date) %>%
    dplyr::compute()
  expect_true(all(cdm$joined_cohorts %>% dplyr::pull(gap_between_index_marker)<= 365))

  loc <- cdm$joined_cohorts %>% dplyr::collect()
  expect_true(all(loc$cohort_start_date == pmin(loc$index_date, loc$marker_date)))
  expect_true(all(loc$cohort_start_date < loc$cohort_end_date))
  CDMConnector::cdmDisconnect(cdm)
  }
)

test_that("one ID against one ID, example 2", {
  skip_on_cran()
  cdm <- mockCohortSymmetry()
  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   indexId=2,
                                   markerTable = "cohort_2",
                                   markerId=2)
  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(omopgenerics::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()

  expect_true(all(abs(loc$marker_date - loc$index_date) <= 365))
  expect_true((loc %>% dplyr::filter(index_name=="cohort_2" & marker_name=="cohort_2") %>% dplyr::tally() %>% dplyr::pull(n)) == 1)
  expect_false((loc %>% dplyr::filter(index_name=="cohort_1" & marker_name=="cohort_2") %>% dplyr::tally() %>% dplyr::pull(n)) > 0)
  CDMConnector::cdmDisconnect(cdm)
}
)

test_that("one ID against one ID, example 3", {
  cdm <- mockCohortSymmetry()
  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   indexId=1,
                                   markerTable = "cohort_2",
                                   markerId=2)

  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(omopgenerics::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()
  expect_true(all(abs(loc$marker_date - loc$index_date) <= 365))
  expect_true((loc %>% dplyr::filter(index_name=="cohort_1" & marker_name=="cohort_2") %>% dplyr::tally() %>% dplyr::pull(n))== 3)
  CDMConnector::cdmDisconnect(cdm)
}
)

### Check multiple entries per person
test_that("multiple entries per person", {
  skip_on_cran()
  indexCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1),
    subject_id = c(1, 1, 1, 1, 2, 2, 2),
    cohort_start_date = as.Date(
      c(
        "2020-04-01", "2021-06-01", "2022-05-22", "2020-01-01", "2010-01-02", "2010-01-03", "2010-01-04"
      )
    ),
    cohort_end_date = cohort_start_date
  )|>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  markerCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1),
    subject_id = c(1, 1, 1, 1, 2, 2, 2),
    cohort_start_date = as.Date(
      c(
        "2020-12-30", "2021-01-01", "2021-05-25", "2022-05-31", "2010-01-03", "2011-01-01", "2012-01-01"
      )
    ),
    cohort_end_date = cohort_start_date
  )|>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  cdm <- mockCohortSymmetry(
    indexCohort = indexCohort,
    markerCohort = markerCohort
  )

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   markerTable = "cohort_2")

  loc <- cdm$joined_cohorts %>% dplyr::collect()
  expect_true(all(loc %>% dplyr::group_by(subject_id) %>% dplyr::tally() %>% dplyr::select(n) == 1))
  expect_true(nrow(loc)==2)

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   markerTable = "cohort_2",
                                   combinationWindow = c(0, 90))

  loc <- cdm$joined_cohorts %>% dplyr::collect()
  expect_true(all(loc %>% dplyr::group_by(subject_id) %>% dplyr::tally() %>% dplyr::select(n) == 1))
  expect_true(nrow(loc)==1)

  CDMConnector::cdmDisconnect(cdm = cdm)
}
)

#### Combination Window
test_that("change combinationWindow one ID against one ID, example 1", {
  skip_on_cran()
  cdm <- mockCohortSymmetry()

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   indexId=1,
                                   markerTable = "cohort_2",
                                   markerId=2,
                                   combinationWindow = c(0,30))

  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(omopgenerics::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()
  expect_true(all(abs(loc$marker_date - loc$index_date) <= 30))
  expect_true(all(0 < abs(loc$marker_date - loc$index_date)))
  expect_true((loc %>% dplyr::filter(index_name=="cohort_1" & marker_name=="cohort_2") %>% dplyr:: tally() %>% dplyr:: pull(n)) == 1)

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   indexId=1,
                                   markerTable = "cohort_2",
                                   markerId=2,
                                   combinationWindow = c(0,Inf))

  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(omopgenerics::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()
  expect_true(all(0 < abs(loc$marker_date - loc$index_date)))
  expect_true((loc %>% dplyr::filter(index_name=="cohort_1" & marker_name=="cohort_2") %>% dplyr:: tally() %>% dplyr:: pull(n)) == 3)

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   indexId=1,
                                   markerTable = "cohort_2",
                                   markerId=2,
                                   combinationWindow = c(7,365))

  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(omopgenerics::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()
  expect_true(all(abs(loc$marker_date - loc$index_date) <= 365))
  expect_true((loc %>% dplyr::filter(index_name=="cohort_1" & marker_name=="cohort_2") %>% dplyr:: tally() %>% dplyr:: pull(n)) == 2)
  expect_true(all(loc$cohort_end_date-loc$cohort_start_date>7 & loc$cohort_end_date-loc$cohort_start_date<365))

  CDMConnector::cdmDisconnect(cdm)
}
)

test_that("change combinationWindow one ID against one ID, example 2", {
  skip_on_cran()
  cdm <- mockCohortSymmetry()

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   indexId=3,
                                   markerTable = "cohort_2",
                                   markerId=1,
                                   combinationWindow = c(0,30))

  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(omopgenerics::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()
  expect_true(all(abs(loc$marker_date - loc$index_date) <= 30))
  expect_true(all(0 < abs(loc$marker_date - loc$index_date)))
  expect_true((loc %>% dplyr::filter(index_name=="cohort_3" & marker_name=="cohort_1") %>% dplyr:: tally() %>% dplyr:: pull(n)) == 1)

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   indexId=3,
                                   markerTable = "cohort_2",
                                   markerId=1,
                                   combinationWindow = c(0,Inf))

  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(omopgenerics::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()
  expect_true(all(0 < abs(loc$marker_date - loc$index_date)))
  expect_true((loc %>% dplyr::filter(index_name=="cohort_3" & marker_name=="cohort_1") %>% dplyr:: tally() %>% dplyr:: pull(n)) == 2)

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   indexId=3,
                                   markerTable = "cohort_2",
                                   markerId=1,
                                   combinationWindow = c(7,Inf))

  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(omopgenerics::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()
  expect_true((loc %>% dplyr::filter(index_name=="cohort_3" & marker_name=="cohort_1") %>% dplyr:: tally() %>% dplyr:: pull(n)) == 1)
  expect_true(all(loc$cohort_end_date-loc$cohort_start_date>7))

  expect_true((loc %>% dplyr::filter(index_name!="cohort_3") %>% dplyr:: tally() %>% dplyr:: pull(n)) == 0)
  expect_true((loc %>% dplyr::filter(marker_name!="cohort_1") %>% dplyr:: tally() %>% dplyr:: pull(n)) == 0)

  CDMConnector::cdmDisconnect(cdm)
}
)

test_that("all IDs against all IDs", {
  skip_on_cran()
  cdm <- mockCohortSymmetry()
  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable = "cohort_1",
                                   markerTable = "cohort_2",
                                   combinationWindow = c(0,Inf))

  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(omopgenerics::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()
  expect_true((cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n)==20))
  expect_true((loc %>% dplyr::filter(index_name=="cohort_1" & marker_name=="cohort_1") %>% dplyr::tally() %>% dplyr::pull(n)) == 2 &
                (loc %>% dplyr::filter(index_name=="cohort_1" & marker_name=="cohort_2") %>% dplyr::tally() %>% dplyr::pull(n)) == 3 &
                (loc %>% dplyr::filter(index_name=="cohort_1" & marker_name=="cohort_3") %>% dplyr::tally() %>% dplyr::pull(n)) == 4 &
                (loc %>% dplyr::filter(index_name=="cohort_2" & marker_name=="cohort_1") %>% dplyr::tally() %>% dplyr::pull(n)) == 0 &
                (loc %>% dplyr::filter(index_name=="cohort_2" & marker_name=="cohort_2") %>% dplyr::tally() %>% dplyr::pull(n)) == 1 &
                (loc %>% dplyr::filter(index_name=="cohort_2" & marker_name=="cohort_3") %>% dplyr::tally() %>% dplyr::pull(n)) == 2 &
                (loc %>% dplyr::filter(index_name=="cohort_3" & marker_name=="cohort_1") %>% dplyr::tally() %>% dplyr::pull(n)) == 2 &
                (loc %>% dplyr::filter(index_name=="cohort_3" & marker_name=="cohort_2") %>% dplyr::tally() %>% dplyr::pull(n)) == 2 &
                (loc %>% dplyr::filter(index_name=="cohort_3" & marker_name=="cohort_3") %>% dplyr::tally() %>% dplyr::pull(n)) == 4)

  # same start date exclusion
  expect_false(1 %in% (loc %>% dplyr::filter(index_name=="cohort_3", marker_name=="cohort_2") %>% dplyr::pull(subject_id)))
  CDMConnector::cdmDisconnect(cdm)
  }
)

test_that("one index (rsp. marker) ID against all marker (rsp. index) IDs", {
  skip_on_cran()
  cdm <- mockCohortSymmetry()
  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   indexId = 1,
                                   markerTable = "cohort_2",
                                   combinationWindow = c(0, Inf)
  )

  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(omopgenerics::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()

  expect_true((loc %>% dplyr::filter(index_name=="cohort_1" & marker_name=="cohort_1") %>% dplyr::tally() %>% dplyr::pull(n) == 2) &
                (loc %>% dplyr::filter(index_name=="cohort_1" & marker_name=="cohort_2") %>% dplyr::tally() %>% dplyr::pull(n) == 3) &
                (loc %>% dplyr::filter(index_name=="cohort_1" & marker_name=="cohort_3") %>% dplyr::tally() %>% dplyr::pull(n) == 4))

  expect_false("cohort_2" %in% (loc %>% dplyr::pull(index_name)))
  expect_false("cohort_3" %in% (loc %>% dplyr::pull(index_name)))

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   markerTable = "cohort_2",
                                   markerId = 1
  )

  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(omopgenerics::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()

  expect_true((loc %>% dplyr::filter(index_name=="cohort_1" & marker_name=="cohort_1") %>% dplyr::tally() %>% dplyr::pull(n) == 2) &
                (loc %>% dplyr::filter(index_name=="cohort_3" & marker_name=="cohort_1") %>% dplyr::tally() %>% dplyr::pull(n) == 1))

  expect_false("cohort_2" %in% (loc %>% dplyr::pull(marker_name)))
  expect_false("cohort_3" %in% (loc %>% dplyr::pull(marker_name)))
  CDMConnector::cdmDisconnect(cdm)
}
)

test_that("a subset of IDs against a subset of IDs", {
  skip_on_cran()
  cdm <- mockCohortSymmetry()
  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   indexId = c(1,2),
                                   markerTable = "cohort_2",
                                   markerId = c(2,3)
  )

  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(omopgenerics::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()

  expect_true((loc %>% dplyr::filter(index_name=="cohort_2" & marker_name=="cohort_3") %>% dplyr::tally() %>% dplyr::pull(n) == 2) &
                (loc %>% dplyr::filter(index_name=="cohort_2" & marker_name=="cohort_2") %>% dplyr::tally() %>% dplyr::pull(n) == 1) &
                (loc %>% dplyr::filter(index_name=="cohort_1" & marker_name=="cohort_2") %>% dplyr::tally() %>% dplyr::pull(n) == 3) &
                (loc %>% dplyr::filter(index_name=="cohort_1" & marker_name=="cohort_3") %>% dplyr::tally() %>% dplyr::pull(n) == 3))

  expect_false("cohort_3" %in% (loc %>% dplyr::pull(index_name)))
  expect_false("cohort_1" %in% (loc %>% dplyr::pull(marker_name)))
  CDMConnector::cdmDisconnect(cdm)
  }
)

test_that("example of changed combinationWindow", {
  skip_on_cran()
  cdm <- mockCohortSymmetry()
  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   indexId = 3,
                                   markerTable = "cohort_2",
                                   markerId = 3
  )

  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 1) # default time, 1 entry

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   indexId = 3,
                                   markerTable = "cohort_2",
                                   markerId = 3,
                                   combinationWindow = c(0,Inf)
  )
  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(omopgenerics::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()
  expect_true(loc %>% dplyr::tally() %>% dplyr::pull(n) == 4) #inf gives more values

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   indexId = 3,
                                   markerTable = "cohort_2",
                                   markerId = 1,
                                   combinationWindow = c(0,Inf)
  )
  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(omopgenerics::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()
  expect_true(loc %>% dplyr::tally() %>% dplyr::pull(n) == 2) #inf gives more values

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   indexId = 3,
                                   markerTable = "cohort_2",
                                   markerId = 1,
                                   combinationWindow = c(0,365)
  )
  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(omopgenerics::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()
  expect_true(loc %>% dplyr::tally() %>% dplyr::pull(n) == 1) #inf gives more values
  expect_true(loc %>% dplyr::select(subject_id) %>% dplyr::pull() == 1)

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   indexId = 3,
                                   markerTable = "cohort_2",
                                   markerId = 1,
                                   combinationWindow = c(7,Inf)
  )
  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(omopgenerics::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()
  expect_true(loc %>% dplyr::tally() %>% dplyr::pull(n) == 1) #inf gives more values
  expect_true(loc %>% dplyr::select(subject_id) %>% dplyr::pull() == 3)

  CDMConnector::cdmDisconnect(cdm)
  })

# priorObservation
test_that("priorObservation and cohortDateRange", {
  skip_on_cran()
  indexCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1),
    subject_id = c(1, 4, 2, 3),
    cohort_start_date = as.Date(
      c(
        "2020-04-01", "2021-06-01", "2022-05-22", "2010-01-01"
      )
    ),
    cohort_end_date = cohort_start_date
  )|>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  markerCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1),
    subject_id = c(1, 3, 4, 2),
    cohort_start_date = as.Date(
      c(
        "2020-12-30", "2010-01-01", "2021-05-25", "2022-05-31"
      )
    ),
    cohort_end_date = cohort_start_date
  )|>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  cdm <- mockCohortSymmetry(
    indexCohort = indexCohort,
    markerCohort = markerCohort
  )

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   markerTable = "cohort_2",
                                   daysPriorObservation = 0,
                                   combinationWindow = c(0,Inf)
  )

  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 3)

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   markerTable = "cohort_2",
                                   daysPriorObservation = 30,
                                   combinationWindow = c(0,365)
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 3)

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   markerTable = "cohort_2",
                                   cohortDateRange = as.Date(c("2020-01-01", NA)),
                                   daysPriorObservation = 0,
                                   combinationWindow = c(0,Inf)
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 3)
  expect_true(all(cdm$joined_cohorts %>% dplyr::pull(subject_id) %in% c(1,2,4)))

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   markerTable = "cohort_2",
                                   cohortDateRange = as.Date(c("2000-01-01", "2022-01-01")),
                                   daysPriorObservation = 30,
                                   combinationWindow = c(0,Inf)
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 2)
  expect_true(all(cdm$joined_cohorts %>% dplyr::pull(subject_id) %in% c(1,4)))

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   markerTable = "cohort_2",
                                   cohortDateRange = as.Date(c("2000-01-01", "2023-01-01")),
                                   daysPriorObservation = 365,
                                   combinationWindow = c(0,Inf)
  )
 # expect_equal(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n), 3)
  expect_true(all(cdm$joined_cohorts %>% dplyr::pull(subject_id) %in% c(1, 2, 4)))
  CDMConnector::cdmDisconnect(cdm)
})

# washoutWindow
test_that("tests involving washout", {
  skip_on_cran()
  indexCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1),
    subject_id = c(1, 1, 1, 1),
    cohort_start_date = as.Date(
      c(
        "2020-04-07", "2020-08-27", "2014-01-01", "2020-01-01"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-04-08", "2020-08-27", "2014-01-01", "2020-01-02"
      )
    )
  )|>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  markerCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1),
    subject_id = c(1, 1, 1, 1),
    cohort_start_date = as.Date(
      c(
        "2021-04-25", "2010-08-26","2022-01-02", "2016-03-01"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2021-04-25","2010-08-27","2022-05-25", "2016-03-14"
      )
    )
  )|>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  cdm <- mockCohortSymmetry(indexCohort = indexCohort,
                            markerCohort = markerCohort)

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   markerTable = "cohort_2",
                                   combinationWindow = c(0,Inf)
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 1)

  earliest_index_date <- cdm$cohort_1 %>%
    dplyr::arrange(cohort_start_date) %>%
    dplyr::filter(dplyr::row_number()==1) %>%
    dplyr::pull(cohort_start_date)

  earliest_marker_date <- cdm$cohort_2 %>%
    dplyr::arrange(cohort_start_date) %>%
    dplyr::filter(dplyr::row_number()==1) %>%
    dplyr::pull(cohort_start_date)

  expect_true(all(cdm$joined_cohorts %>% dplyr::pull(index_date) == earliest_index_date,
                  cdm$joined_cohorts %>% dplyr::pull(marker_date) == earliest_marker_date))

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   markerTable = "cohort_2"
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 0) #combinationWindow fails

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   markerTable = "cohort_2",
                                   daysPriorObservation = 3650,
                                   combinationWindow = c(0, Inf)
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 0) #insufficient prior_obs

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   cohortDateRange = as.Date(c("2000-01-01", NA)),
                                   indexTable ="cohort_1",
                                   markerTable = "cohort_2",
                                   daysPriorObservation = 0,
                                   combinationWindow = c(0,Inf)
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 1)
  expect_true(cdm$joined_cohorts %>% dplyr::pull(cohort_start_date) >= as.Date("2000-01-01"))

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   markerTable = "cohort_2",
                                   washoutWindow = 365,
                                   combinationWindow = c(0,Inf)
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 1)
  index_date <- cdm$joined_cohorts %>% dplyr::pull(index_date)
  test_index <- cdm$cohort_1 %>% dplyr::filter(cohort_start_date<=as.Date(index_date)) %>% dplyr::collect() %>% dplyr::filter(cohort_start_date+365 >= as.Date(index_date)) %>% dplyr::collect()
  expect_true(test_index %>% dplyr::tally() %>% dplyr::pull(n) == 1)

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   markerTable = "cohort_2",
                                   cohortDateRange = as.Date(c("2002-01-01", NA)),
                                   washoutWindow = 365,
                                   combinationWindow = c(0,Inf)
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 1)

  expect_error(
    cdm <- generateSequenceCohortSet(cdm,
                                     name = "joined_cohorts",
                                     indexTable ="cohort_1",
                                     markerTable = "cohort_2",
                                     cohortDateRange = as.Date(c("2020-04-01", NA)),
                                     washoutWindow = 365,
                                     combinationWindow = c(0,Inf)
    )
  )

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   markerTable = "cohort_2",
                                   washoutWindow = 365,
                                   combinationWindow = c(0,Inf)
  )

  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 1)
  expect_identical(
    cdm$joined_cohorts %>%
      dplyr::select(cohort_start_date) %>%
      dplyr::pull(cohort_start_date) %>%
      as.Date(),
    as.Date("2010-08-26")
  )

  expect_identical(
    cdm$joined_cohorts %>%
      dplyr::select(cohort_end_date) %>%
      dplyr::pull(cohort_end_date) %>%
      as.Date(),
    as.Date("2014-01-01")
  )

  CDMConnector::cdmDisconnect(cdm = cdm)
})

test_that("tests involving indexMarkerGap", {
  skip_on_cran()
  indexCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1),
    subject_id = c(1, 1, 2, 2),
    cohort_start_date = as.Date(
      c(
        "2020-04-07", "2023-08-27", "2014-01-01", "2020-01-01"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2021-04-08", "2023-08-27", "2014-01-01", "2021-01-02"
      )
    )
  )|>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  markerCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1),
    subject_id = c(1, 1, 2, 2),
    cohort_start_date = as.Date(
      c(
        "2021-04-25", "2022-08-26","2022-01-02", "2015-03-01"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2021-04-25","2023-08-27","2022-05-25", "2016-03-14"
      )
    )
  )|>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  cdm <- mockCohortSymmetry(indexCohort = indexCohort,
                                            markerCohort = markerCohort)

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   markerTable = "cohort_2",
                                   combinationWindow = c(0,Inf)
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n)==2)

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   markerTable = "cohort_2",
                                   combinationWindow = c(0,365)
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n)==0)

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   markerTable = "cohort_2",
                                   combinationWindow = c(0, Inf),
                                   indexMarkerGap = 730
                                   )

  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n)==2)

  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   markerTable = "cohort_2",
                                   combinationWindow = c(0, Inf),
                                   indexMarkerGap = 15
  )

  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n)==0)
  cdm <- generateSequenceCohortSet(cdm,
                                   name = "joined_cohorts",
                                   indexTable ="cohort_1",
                                   markerTable = "cohort_2",
                                   combinationWindow = c(0, Inf),
                                   indexMarkerGap = 20
  )

  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n)==1)

  CDMConnector::cdmDisconnect(cdm)
}
)
################################# Input Validation ################################
test_that("unsuccessful examples - Inf prior observation", {
  skip_on_cran()
  cdm <- mockCohortSymmetry()

  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable ="cohort1",
                                         markerTable = "cohort2",
                                         daysPriorObservation = Inf
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("warning examples - name not in the right form", {
  skip_on_cran()
  cdm <- mockCohortSymmetry()
  expect_warning(generateSequenceCohortSet(cdm,
                                         name = "joinCohorts",
                                         indexTable = "cohort_1",
                                         markerTable = "cohort_2",
                                         daysPriorObservation = 0
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("unsuccessful examples - indexTable not strings", {
  skip_on_cran()
  cdm <- mockCohortSymmetry()
  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable = cohort1,
                                         markerTable = "cohort2",
                                         daysPriorObservation = 0
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("unsuccessful examples - markerTable not strings", {
  skip_on_cran()
  cdm <- mockCohortSymmetry()
  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable = "cohort1",
                                         markerTable = cohort2,
                                         daysPriorObservation = 0
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("unsuccessful examples - daysPriorObservation is not numeric", {
  skip_on_cran()
  cdm <- mockCohortSymmetry()
  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable = "cohort1",
                                         markerTable = "cohort2",
                                         daysPriorObservation = "seven"
  ))
  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable = "cohort1",
                                         markerTable = "cohort2",
                                         daysPriorObservation = 2.5
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("unsuccessful examples - Ids outside of range", {
  skip_on_cran()
  cdm <- mockCohortSymmetry()
  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable = "cohort1",
                                         markerTable = "cohort2",
                                         indexId = 2
  ))
  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable = "cohort1",
                                         markerTable = "cohort2",
                                         markerId = 2
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("unsuccessful examples - tables not in the CDM", {
  skip_on_cran()
  cdm <- mockCohortSymmetry()
  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable = "cohort1",
                                         markerTable = "cohort3",
                                         indexId = 2
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("mock db: unsuccessful examples - tables not in the right format", {
  skip_on_cran()
  cdm <- mockCohortSymmetry()
  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable = "cohort1",
                                         markerTable = "drug_exposure"
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("mock db: unsuccessful examples - tables not in the right format", {
  skip_on_cran()
  cdm <- mockCohortSymmetry()
  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable = "cohort1",
                                         markerTable = "drug_exposure"
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("unsuccessful examples - negative parameters", {
  skip_on_cran()
  cdm <- mockCohortSymmetry()
  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable = "cohort1",
                                         markerTable = "drug_exposure",
                                         daysPriorObservation = -100
  ))
  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable = "cohort1",
                                         markerTable = "drug_exposure",
                                         washoutWindow = -100
  ))
  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable = "cohort1",
                                         markerTable = "drug_exposure",
                                         combinationWindow = c(-200,-100)
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("generateSequenceCohortSet - inputValidation", {
  skip_on_cran()
  cdm <- mockCohortSymmetry()
  expect_error(
    generateSequenceCohortSet(
      list(),
      name = "joined_cohorts",
      indexTable = "cohort_1",
      markerTable = "cohort_2",
      cohortDateRange = as.Date(c("2002-01-01", NA)),
      washoutWindow = 365,
      combinationWindow = c(0,Inf)
    )
  )
  expect_error(
    generateSequenceCohortSet(
      cdm = cdm,
      name = "joined_cohorts",
      indexTable = "cohort_1",
      indexId = 90,
      markerTable = "cohort_2",
      cohortDateRange = as.Date(c("2002-01-01", NA)),
      washoutWindow = 365,
      combinationWindow = c(0,Inf)
    )
  )
  expect_error(
    generateSequenceCohortSet(
      cdm = cdm,
      name = "joined_cohorts",
      indexTable = "cohort_1",
      markerTable = "cohort_2",
      cohortDateRange = c(as.Date("2002-01-01"),1),
      washoutWindow = 365,
      combinationWindow = c(0, Inf)
    ))
  expect_error(
    generateSequenceCohortSet(
      cdm = cdm,
      name = "joined_cohorts",
      indexTable = "cohort_1",
      markerTable = "cohort_2",
      cohortDateRange = as.Date(c("2002-01-01", NA)),
      washoutWindow = -1,
      daysPriorObservation = Inf,
      combinationWindow = c(0, Inf)
    )
  )
  expect_error(
    generateSequenceCohortSet(
      cdm = cdm,
      name = "joined_cohorts",
      indexTable = "cohort_1",
      markerTable = "cohort_2",
      cohortDateRange = as.Date(c("2002-01-01", NA)),
      combinationWindow = c(80, 40)
    )
  )
  expect_error(
    generateSequenceCohortSet(
      cdm = cdm,
      name = "joined_cohorts",
      indexTable = "cohort_1",
      markerTable = "cohort_2",
      cohortDateRange = as.Date(c("2002-01-01", NA)),
      combinationWindow = c(Inf, Inf)
    )
  )
  CDMConnector::cdmDisconnect(cdm)
})
