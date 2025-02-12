test_that("eunomia - generateSequenceCohortSet", {
  skip_on_cran()

  if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") {
    Sys.setenv("EUNOMIA_DATA_FOLDER" = tempdir())
  }
  if (!dir.exists(Sys.getenv("EUNOMIA_DATA_FOLDER"))) {
    dir.create(Sys.getenv("EUNOMIA_DATA_FOLDER"))
  }
  if (!CDMConnector::eunomiaIsAvailable()) {
    invisible(utils::capture.output(CDMConnector::downloadEunomiaData(pathToData = Sys.getenv("EUNOMIA_DATA_FOLDER"))))
  }

  con <- DBI::dbConnect(duckdb::duckdb(),
                        dbdir = CDMConnector::eunomiaDir())
  cdm <- CDMConnector::cdmFromCon(con, cdmSchema = "main", writeSchema = "main")

  index_drug <- CodelistGenerator::getDrugIngredientCodes(cdm = cdm,
                                                          name = "celecoxib")
  marker_drug <- CodelistGenerator::getDrugIngredientCodes(cdm = cdm,
                                                           name = "aspirin")

  cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = "cohort1",
    conceptSet = index_drug
  )
  cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = "cohort2",
    conceptSet = marker_drug
  )

  expect_no_error(
    cdm <- generateSequenceCohortSet(cdm,
                                     name = "joined_cohorts",
                                     indexTable ="cohort1",
                                     markerTable = "cohort2",
                                     combinationWindow = c(0,Inf)))

 expect_true(nrow(cdm$joined_cohorts %>% dplyr::collect()) > 0)

 expect_no_error(
   res <- summariseSequenceRatios(cohort = cdm$joined_cohorts)
 )

 expect_true(nrow(res) > 0)

 expect_no_error(
   nice_table <- tableSequenceRatios(res)
 )

 expect_no_error(
   nice_table <- tableSequenceRatios(res, type = "flextable")
 )

 expect_no_error(
   nice_table <- tableSequenceRatios(res, type = "tibble")
 )

  CDMConnector::cdmDisconnect(cdm)
})

test_that("check redundant_fun", {
  skip_on_cran()
  expect_no_error(
    redundant_fun()
  )
})
