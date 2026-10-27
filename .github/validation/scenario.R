## Small illustrative scenario shared by the v44/v49 integration checks.
makeValidationScenario <- function(version) {
  baseList <- list(
    OMVersion = version, demography = list(), monitoring = list(),
    interventions = list(), healthSystem = list(), entomology = list(),
    model = list()
  )
  baseList <- defineDemography(
    baseList, name = "Validation", popSize = 200L, maximumAgeYrs = 90,
    lowerbound = 0,
    poppercent = c(
      3.474714994, 12.76004028, 14.52151394, 12.75565434, 10.83632374,
      8.393312454, 7.001421452, 5.800587654, 5.102136612, 4.182561874,
      3.339409351, 2.986112356, 2.555766582, 2.332763433, 1.77400255,
      1.008525491, 0.74167341, 0.271863401, 0.161614642
    ),
    upperbound = c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90)
  )
  measures <- c(
    "nHost", "nPatent", "nUncomp", "nSevere", "nNewInfections",
    "inputEIR", "simulatedEIR"
  )
  if (version == 49L) {
    measures <- c(measures, "nNewInfections_Imported", "nNewInfections_Introduced",
                  "nNewInfections_Indigenous", "nCMDTReport",
                  "nSevereWithoutComorbidities", "expectedSevereWithoutComorbidities")
  }
  baseList$diagnostics <- list(diagnostic = list(
    name = "survey", deterministic = list(minDensity = 100)
  ))
  baseList$monitoring <- list(
    name = "Annual validation surveys", startDate = "2000-01-01",
    continuous = monitoringContinuousGen(
      period = 1, options = list(
        name = c("input EIR", "simulated EIR"), value = c("true", "true")
      )
    ),
    SurveyOptions = monitoringSurveyOptionsGen(
      options = list(name = measures, value = rep("true", length(measures)))
    ),
    surveys = monitoringSurveyTimesGen(
      startDate = "2000-01-01", endDate = "2001-12-31", interval = "yearly",
      simStart = "2000-01-01", diagnostic = "survey"
    ),
    ageGroup = surveyAgeGroupsGen(0, c(5, 90))
  )
  baseList <- defineEntomology(
    baseList,
    seasonalityParameters = list(gambiae_ss = list(
      annualEIR = 1, propInfected = 0.078, propInfectious = 0.021,
      seasonality = c(5, 4, 3, 2, 1, 2, 3, 5, 8, 13, 11, 7)
    )),
    mosquitoParameters = list(gambiae_ss = list(
      mosqRestDuration = list(value = 3),
      extrinsicIncubationPeriod = list(value = 11),
      mosqLaidEggsSameDayProportion = list(value = 0.313),
      mosqSeekingDuration = list(value = 0.33),
      mosqSurvivalFeedingCycleProbability = list(value = 0.623),
      mosqProbBiting = list(mean = 0.95, variance = 0),
      mosqProbFindRestSite = list(mean = 0.95, variance = 0),
      mosqProbResting = list(mean = 0.99, variance = 0),
      mosqProbOvipositing = list(mean = 0.88),
      mosqHumanBloodIndex = list(mean = 0.939)
    )), scaledAnnualEIR = 10
  )
  baseList <- defineImportedInfections(baseList, value = 10, time = 0)
  baseList <- defineHealthSystem(
    baseList, pSeekOfficialCareUncomplicated1 = 0.4,
    pSeekOfficialCareUncomplicated2 = 0.4
  )
  write_end_compat(baseList, seed = "@seed@")
}
