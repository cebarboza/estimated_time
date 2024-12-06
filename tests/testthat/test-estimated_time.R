test_that("TestGenerator readPatients.csv ", {
  # TestGenerator reads a folder of CSVs or an Excel document.
  # Each CSV or sheet in the Excel represents a table from the CDM, 
  # e.g. condition_occurrence, drug_exposure, etc.
  
  filePath <- testthat::test_path("mimicIVsubset")
  TestGenerator::readPatients.csv(filePath = filePath,
                                  testName = "mimicIVsubset",
                                  cdmVersion = "5.3",
                                  reduceLargeIds = FALSE)

  # or read the Excel file with remifentanil patients only
  filePath <- testthat::test_path("icu_sample_population_remifentanil.xlsx")
  TestGenerator::readPatients.xl(filePath = filePath,
                                  testName = "remifentanil",
                                  cdmVersion = "5.3")
  
  
  # Generates CDM with sample population and complete vocabulary
  
  cdm <- TestGenerator::patientsCDM(testName = "mimicIVsubset")
  
  cdm <- TestGenerator::patientsCDM(testName = "remifentanil")
  
  
  # Generates a cohort set with patients who have been exposed to remifentanil
  
  remifentanil_cohort_set <- here::here("concept_set", "remifentanil.json")  
  
  concept_list <- CodelistGenerator::codesFromConceptSet(remifentanil_cohort_set, cdm)
  
  cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(cdm = cdm,
                                                           name = "remifentanil",
                                                           conceptSet = concept_list,
                                                           gapEra = 1,
                                                           durationRange = lifecycle::deprecated(),
                                                           imputeDuration = lifecycle::deprecated(),
                                                           priorUseWashout = lifecycle::deprecated(),
                                                           priorObservation = lifecycle::deprecated(),
                                                           cohortDateRange = lifecycle::deprecated(),
                                                           limit = lifecycle::deprecated())
  
  remifentanil_exposed_time <- cdm$remifentanil %>% 
    DrugUtilisation::addExposedTime(conceptSet = concept_list, 
                                    gapEra = 1,
                                    indexDate = "cohort_start_date",
                                    censorDate = "cohort_end_date")
  
  remifentanil_exposed_days <- remifentanil_exposed_time %>%
    pull(exposed_time_remifentanil) %>% 
    sum()
  
  expect_equal(remifentanil_exposed_days, 30)
  
  sum(remifentanil_exposed_time$exposed_time_remifentanil %>% collect())
  
  remifentanil_summary <- DrugUtilisation::summariseDrugUtilisation(cdm$remifentanil,
                                                                    conceptSet = concept_list,
                                                                    indexDate = "cohort_start_date",
                                                                    censorDate = "cohort_end_date",
                                                                    ingredientConceptId = 19016749) 
  
  

  
  
})
