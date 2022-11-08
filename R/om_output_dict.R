### Information about OpenMalaria's output


## Load order
##' @include cache.R
NULL


##' @title Open Malaria output dictionary
##' @description A dictionary which provides translations for the following
##'   outputs of Open Malaria:
##'
##'   - Survey measure numbers to names
##'
##'   - If a measure's output can be separated by age group, cohort, genotype,
##'     vector species or drug ID
##'
##'   - Whether measures are aggregated between survey dates or if their values
##'     is a snapshot
##'
##' See
##' https://github.com/SwissTPH/openmalaria/wiki/MonitoringOptions#survey-measures
##' @export
omOutputDict <- function() {
  dict <- data.table::data.table(
    measure_index = as.integer(c(
      0, 1, 2, 3, 4, 5, 6, 7, 8,
      10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
      20, 21, 22, 23, 24, 25, 26, 27,
      30, 31, 32, 33, 34, 35, 36,
      43,
      52, 53, 54, 55, 56, 57, 58, 59,
      60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
      70, 71, 72, 73, 74, 75, 76, 77, 78, 79
    )),
    measure_name = c(
      ## 0 - 8
      "nHost", "nInfect", "nExpectd", "nPatent", "sumLogPyrogenThres",
      "sumlogDens", "totalInfs", "nTransmit", "totalPatentInf",

      ## 10s
      "sumPyrogenThresh", "nTreatments1", "nTreatments2", "nTreatments3",
      "nUncomp", "nSevere", "nSeq", "nHospitalDeaths", "nIndDeaths",
      "nDirDeaths",

      ## 20s
      "nEPIVaccinations", "allCauseIMR", "nMassVaccinations", "nHospitalRecovs",
      "nHospitalSeqs", "nIPTDoses", "annAvgK", "nNMFever",

      ## 30s
      "innoculationsPerAgeGroup", "Vector_Nv0", "Vector_Nv", "Vector_Ov",
      "Vector_Sv", "inputEIR", "simulatedEIR",

      ## 40s
      "nNewefections",

      ## 50s
      "nMDAs", "nNmfDeaths", "nAntibioticTreatments", "nMassScreenings",
      "nMassGVI", "nCtsIRS", "nCtsGVI", "nCtsMDA",

      ## 60s
      "nCtsScreenings", "nSubPopRemovalTooOld", "nSubPopRemovalFirstEvent",
      "nLiverStageTreatments", "nTreatDiagnostics", "nMassRecruitOnly",
      "nCtsRecruitOnly", "nTreatDeployments", "sumAge", "nInfectByGenotype",

      ## 70s
      "nPatentByGenotype", "logDensByGenotype", "nHostDrugConcNonZero",
      "sumLogDrugConcNonZero", "expectedDirectDeaths", "expectedHospitalDeaths",
      "expectedIndirectDeaths", "expectedSequelae", "expectedSevere",
      "innoculationsPerVector"
    ),
    age_group = c(
      ## 0 - 8
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE,
      ## 10s
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      ## 20s
      TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE,
      ## 30s
      TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      ## 40s
      TRUE,
      ## 50s
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      ## 60s
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      ## 70s
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE
    ),
    cohort = c(
      ## 0 - 8
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE,
      ## 10s
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      ## 20s
      TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE,
      ## 30s
      TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      ## 40s
      TRUE,
      ## 50s
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      ## 60s
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      ## 70s
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE
    ),
    genotype = c(
      ## 0 - 8
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE,
      ## 10s
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      ## 20s
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      ## 30s
      TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE,
      ## 40s
      FALSE,
      ## 50s
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      ## 60s
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      ## 70s
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
    ),
    vector_species = c(
      ## 0 - 8
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      ## 10s
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      ## 20s
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      ## 30s
      FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE,
      ## 40s
      FALSE,
      ## 50s
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      ## 60s
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      ## 70s
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE
    ),
    drug_ID = c(
      ## 0 - 8
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      ## 10s
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      ## 20s
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      ## 30s
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      ## 40s
      FALSE,
      ## 50s
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      ## 60s
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      ## 70s
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
    ),
    aggregated = c(
      ## 0 - 8
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      ## 10s
      FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      ## 20s
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE,
      ## 30s
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      ## 40s
      TRUE,
      ## 50s
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      ## 60s
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE,
      ## 70s
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
    )
  )
  return(dict)
}
