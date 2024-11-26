test_that("tableSequenceRatios - gt output", {
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  cdm <- mockCohortSymmetry()
  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   indexTable = "cohort_1",
                                   indexId = 1,
                                   markerTable = "cohort_2",
                                   markerId = 3,
                                   name = "joined_cohort")

  res <- summariseSequenceRatios(cohort = cdm$joined_cohort, minCellCount = 0)

  gtResult <- tableSequenceRatios(res)
  expect_true("gt_tbl" %in% (gtResult %>% class()))

  expect_no_error(
    tableSequenceRatios(res, header = "index_cohort_name")
  )

  expect_warning(
    tableTemporalSymmetry(res)
  )

  expect_error(
    tableSequenceRatios(res, header = "cdm_name")
  )

  expect_no_error(
    tableSequenceRatios(res,
                        header = "index_cohort_name",
                        groupColumn = "cdm_name")
  )

  expect_no_error(
    tableSequenceRatios(res,
                        header = "index_cohort_name",
                        groupColumn = character(),
                        hide = "cdm_name")
  )

  CDMConnector::cdmDisconnect(cdm)
})

test_that("tableSequenceRatios - tibble output", {
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

  res <- summariseSequenceRatios(cohort = cdm$joined_cohort, minCellCount = 0)

  expect_warning(
    tableTemporalSymmetry(res, type = "tibble")
  )

  tibble_res <- tableSequenceRatios(res, type = "tibble")

  expect_true("data.frame" %in% (tibble_res %>% class()))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("tableSequenceRatios - flextable output", {
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

  res <- summariseSequenceRatios(cohort = cdm$joined_cohort, minCellCount = 0)

  expect_warning(
    tableTemporalSymmetry(res, type = "flextable")
  )

  flextable_res <- tableSequenceRatios(res, type = "flextable")

  expect_true("flextable" %in% (flextable_res %>% class()))

  expect_no_error(
    tableSequenceRatios(res,
                        type = "flextable",
                        header = "index_cohort_name")
  )

  expect_error(
    tableSequenceRatios(res,
                        type = "flextable",
                        header = "cdm_name")
  )

  expect_no_error(
    tableSequenceRatios(res,
                        type = "flextable",
                        header = "index_cohort_name",
                        groupColumn = "cdm_name")
  )

  expect_no_error(
    tableSequenceRatios(res,
                        type = "flextable",
                        header = "index_cohort_name",
                        groupColumn = character(),
                        hide = "cdm_name")
  )

  CDMConnector::cdmDisconnect(cdm)
})
