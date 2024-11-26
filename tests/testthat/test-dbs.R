test_that("SQL Server", {
  skip_on_cran()
  skip_if(Sys.getenv("SQL_SERVER_DRIVER") == "")

  db <- DBI::dbConnect(odbc::odbc(),
                      Driver   = Sys.getenv("SQL_SERVER_DRIVER"),
                      Server   = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
                      Database = Sys.getenv("CDM5_SQL_SERVER_CDM_DATABASE"),
                      UID      = Sys.getenv("CDM5_SQL_SERVER_USER"),
                      PWD      = Sys.getenv("CDM5_SQL_SERVER_PASSWORD"),
                      TrustServerCertificate="yes",
                      Port     = Sys.getenv("CDM5_SQL_SERVER_PORT"))
  cdm_schema <- strsplit(Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA"), "\\.")[[1]]
  write_schema<- strsplit(Sys.getenv("CDM5_SQL_SERVER_SCRATCH_SCHEMA"), "\\.")[[1]]

cdm <- CDMConnector::cdm_from_con(db, cdm_schema = cdm_schema,
                                  write_schema =  c(schema = write_schema,
                                                    prefix = "ssa_p_"))

index_drug <- CodelistGenerator::getDrugIngredientCodes(cdm = cdm,
                                                        name = "celecoxib")
marker_drug <- CodelistGenerator::getDrugIngredientCodes(cdm = cdm,
                                                         name = "aspirin")

cdm <- CDMConnector::generate_concept_cohort_set(cdm = cdm,
                                                 concept_set = index_drug,
                                                 name = "csyim_index",
                                                 limit = "all",
                                                 overwrite = TRUE)
cdm <- CDMConnector::generate_concept_cohort_set(cdm = cdm,
                                                 concept_set = marker_drug,
                                                 name = "csyim_marker",
                                                 limit = "all",
                                                 overwrite = TRUE)

expect_no_error(cdm <- generateSequenceCohortSet(cdm,
                                                                 name = "joined_cohorts",
                                                           indexTable ="csyim_index",
                                                           markerTable = "csyim_marker",
                                                           combinationWindow = c(0,Inf)))

expect_true(nrow(cdm$joined_cohorts %>% dplyr::collect()) > 0)

CDMConnector::dropTable(cdm, tidyselect::starts_with("csyim"))

CDMConnector::cdm_disconnect(cdm)
})

test_that("Redshift", {
  skip_on_cran()
  skip_if(Sys.getenv("CDM5_REDSHIFT_DBNAME") == "")
  db <- DBI::dbConnect(RPostgres::Redshift(),
                       dbname   = Sys.getenv("CDM5_REDSHIFT_DBNAME"),
                       host     = Sys.getenv("CDM5_REDSHIFT_HOST"),
                       port     = Sys.getenv("CDM5_REDSHIFT_PORT"),
                       user     = Sys.getenv("CDM5_REDSHIFT_USER"),
                       password = Sys.getenv("CDM5_REDSHIFT_PASSWORD"))
  cdm_schema <-  Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA")
  write_schema<- Sys.getenv("CDM5_REDSHIFT_SCRATCH_SCHEMA")

  cdm <- CDMConnector::cdm_from_con(db, cdm_schema = cdm_schema,
                                    write_schema =  write_schema)

  index_drug <- CodelistGenerator::getDrugIngredientCodes(cdm = cdm,
                                                          name = "celecoxib")
  marker_drug <- CodelistGenerator::getDrugIngredientCodes(cdm = cdm,
                                                           name = "aspirin")

  cdm <- CDMConnector::generate_concept_cohort_set(cdm = cdm,
                                                   concept_set = index_drug,
                                                   name = "csyim_index",
                                                   limit = "all",
                                                   overwrite = TRUE)
  cdm <- CDMConnector::generate_concept_cohort_set(cdm = cdm,
                                                   concept_set = marker_drug,
                                                   name = "csyim_marker",
                                                   limit = "all",
                                                   overwrite = TRUE)

  expect_no_error(cdm <- generateSequenceCohortSet(cdm,
                                                   name = "joined_cohorts",
                                                   indexTable ="csyim_index",
                                                   markerTable = "csyim_marker",
                                                   combinationWindow = c(0,Inf)))

  expect_true(nrow(cdm$joined_cohorts %>% dplyr::collect()) > 0)

  CDMConnector::dropTable(cdm, tidyselect::starts_with("csyim"))

  CDMConnector::cdm_disconnect(cdm)
})
