test_that("tableTemporalSymmetry - gt output", {
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  cdm <- mockCohortSymmetry()
  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   indexTable = "cohort_1",
                                   indexId = 1,
                                   markerTable = "cohort_2",
                                   markerId = 3,
                                   name = "joined_cohort")

  res <- summariseTemporalSymmetry(cohort = cdm$joined_cohort, minCellCount = 0)

  gtResult <- tableTemporalSymmetry(res)
  expect_true("gt_tbl" %in% (gtResult %>% class()))

  expect_no_error(
    tableTemporalSymmetry(res, header = "index_cohort_name")
  )

  expect_warning(
    tableSequenceRatios(res)
  )

  expect_error(
    tableTemporalSymmetry(res, header = "cdm_name")
  )

  expect_no_error(
    tableTemporalSymmetry(res,
                        header = "index_cohort_name",
                        groupColumn = "cdm_name")
  )

  expect_no_error(
    tableTemporalSymmetry(res,
                        header = "index_cohort_name",
                        groupColumn = character(),
                        hide = "cdm_name")
  )

  CDMConnector::cdmDisconnect(cdm)
})

test_that("tableTemporalSymmetry - tibble output", {
  skip_on_cran()
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  cdm <- mockCohortSymmetry()
  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   indexTable = "cohort_1",
                                   indexId = 1,
                                   markerTable = "cohort_2",
                                   markerId = 3,
                                   name = "joined_cohort")

  res <- summariseTemporalSymmetry(cohort = cdm$joined_cohort, minCellCount = 0)

  tibble_res <- tableTemporalSymmetry(res, type = "tibble")

  expect_warning(
    tableSequenceRatios(res, type = "tibble")
  )

  expect_true("data.frame" %in% (tibble_res %>% class()))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("tableTemporalSymmetry - flextable output", {
  skip_on_cran()
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  cdm <- mockCohortSymmetry()
  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   indexTable = "cohort_1",
                                   indexId = 1,
                                   markerTable = "cohort_2",
                                   markerId = 3,
                                   name = "joined_cohort")

  res <- summariseTemporalSymmetry(cohort = cdm$joined_cohort, minCellCount = 0)

  flextable_res <- tableTemporalSymmetry(res, type = "flextable")

  expect_warning(
    tableSequenceRatios(res, type = "flextable")
  )

  expect_true("flextable" %in% (flextable_res %>% class()))

  expect_no_error(
    tableTemporalSymmetry(res,
                          type = "flextable",
                          header = "index_cohort_name")
  )

  expect_error(
    tableTemporalSymmetry(res,
                          type = "flextable",
                          header = "cdm_name")
  )

  expect_no_error(
    tableTemporalSymmetry(res,
                          type = "flextable",
                          header = "index_cohort_name",
                          groupColumn = "cdm_name")
  )

  expect_no_error(
    tableTemporalSymmetry(res,
                          type = "flextable",
                          header = "index_cohort_name",
                          groupColumn = character(),
                          hide = "cdm_name")
  )

  CDMConnector::cdmDisconnect(cdm)
})
