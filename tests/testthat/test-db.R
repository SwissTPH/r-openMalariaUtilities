## Setup
clearCache()
rootDir <- file.path(tempdir(), "test-db")
if (dir.exists(rootDir)) {
  unlink(rootDir, recursive = TRUE)
}
dir.create(rootDir)

putCache("rootDir", rootDir)

test_that(".createDB works", {
  testcon <- .createDB("test")

  ## DB connection exists
  expect_s4_class(
    object = testcon, class = "SQLiteConnection"
  )

  ## DB exist
  expect_equal(
    object = file.exists(file.path(rootDir, "test.sqlite")), expected = TRUE
  )

  ## Close and open connection again
  DBI::dbDisconnect(testcon)
  rm(testcon)
  testcon <- .createDB("test")
  expect_s4_class(
    object = testcon, class = "SQLiteConnection"
  )
})

test_that(".createTables works", {
  testcon <- .createDB("test")

  ## Create tables
  .createTables(testcon)

  expected <- c(
    "experiments", "placeholders", "results", "scenarios", "scenarios_metadata"
  )
  actual <- DBI::dbListTables(testcon)
  expect_equal(actual, expected)

  ## Close connection
  DBI::dbDisconnect(testcon)
})

test_that("omOutputDict works", {
  expected <- data.table::data.table(
    measure_index = as.integer(c(
      0, 1, 2, 3, 4, 5, 6, 7, 8,
      10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
      20, 21, 22, 23, 24, 25, 26, 27,
      30, 31, 32, 33, 34, 35, 36, 39,
      40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
      50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
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
      "Vector_Sv", "inputEIR", "simulatedEIR", "Clinical_RDTs",

      ## 40s
      "Clinical_DrugUsage", "Clinical_FirstDayDeaths",
      "Clinical_HospitalFirstDayDeaths", "nNewefections", "nMassITNs",
      "nEPI_ITNs", "nMassIRS", "nMassVA", "Clinical_Microscopy",
      "Clinical_DrugUsageIV",

      ## 50s
      "nAddedToCohort", "nRemovedFromCohort", "nMDAs", "nNmfDeaths",
      "nAntibioticTreatments", "nMassScreenings", "nMassGVI", "nCtsIRS",
      "nCtsGVI", "nCtsMDA",

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
    incident = c(
      ## 0 - 8
      TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA, NA,

      ## 10s
      FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,

      ## 20s
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE,

      ## 30s
      TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE,

      ## 40s
      NA, NA, NA, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,

      ## 50s
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,

      ## 60s
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE,

      ## 70s
      TRUE, FALSE, TRUE, NA, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE
    ),
    third_dimension = c(
      ## 0 - 8
      "age_group", "age_group", "age_group", "age_group", "age_group",
      "age_group", "age_group", NA, "age_group",

      ## 10s
      "age_group", "age_group", "age_group", "age_group", "age_group",
      "age_group", "age_group", "age_group", "age_group", "age_group",

      ## 20s
      "age_group", NA, "age_group", "age_group", "age_group", "age_group", NA,
      "age_group",

      ## 30s
      "age_group", "vector_species", "vector_species", "vector_species",
      "vector_species", NA, NA, NA,

      ## 40s
      "drug_id", "age_group", "age_group", "age_group", "age_group",
      "age_group", "age_group", "age_group", NA, "drug_id",

      ## 50s
      "age_group", "age_group", "age_group", "age_group", "age_group",
      "age_group", "age_group", "age_group", "age_group", "age_group",

      ## 60s
      "age_group", "age_group", "age_group", "age_group", "age_group",
      "age_group", "age_group", "age_group", "age_group", "age_group",

      ## 70s
      "age_group", "age_group", "age_group", "age_group", "age_group",
      "age_group", "age_group", "age_group", "age_group", NA
    )
  )

  actual <- omOutputDict()
  expect_equal(actual, expected)
})

test_that("readOutputFile works", {
  testdata <- read.delim(
    text = "
1	1	0	71
1	2	0	57
1	3	0	151
", sep = "\t", header = FALSE
  )
  write.table(
    testdata,
    file = file.path(rootDir, "test.txt"), row.names = FALSE
  )
  expected <- data.table::data.table(testdata)
  colnames(expected) <- c(
    "survey_date", "third_dimension", "measure", "value"
  )
  expected <- expected[, survey_date := c("2000-01-16", "2000-01-16", "2000-01-16")]
  expected <- expected[, measure := c("nHost", "nHost", "nHost")]
  expected <- expected[, third_dimension := as.character(third_dimension)]

  dates <- .xmlMonitoringTimeRegularSeq(
    "2000-01-01", "2000-03-20",
    daysFilter = 5, dateFilter = "monthly"
  )
  putCache(
    "surveyTimes",
    data.table::data.table(number = seq.int(nrow(dates)), dates)
  )

  actual <- readOutputFile(file.path(rootDir, "test.txt"))
  expect_equal(actual, expected)
})

test_that(".addExpToDB works", {
  testcon <- .createDB("test")

  .addExpToDB(testcon, "test")

  expected <- data.frame(experiment_id = 1, name = "test")
  actual <- DBI::dbReadTable(testcon, "experiments")
  expect_equal(actual, expected)

  ## Handle duplicate entries
  expect_warning(
    .addExpToDB(
      testcon, "test",
      method = "ignore"
    ),
    regexp = "and has been skipped"
  )
  expected <- data.frame(experiment_id = 1, name = "test")
  actual <- DBI::dbReadTable(testcon, "experiments")
  expect_equal(actual, expected)

  expect_warning(
    .addExpToDB(
      testcon, "test",
      method = "replace"
    ),
    regexp = "and has been replaced"
  )
  expected <- data.frame(experiment_id = 1, name = "test")
  actual <- DBI::dbReadTable(testcon, "experiments")
  expect_equal(actual, expected)

  ## Close connection
  DBI::dbDisconnect(testcon)
})

test_that(".addScenToDB works", {
  testcon <- .createDB("test")

  scens <- data.frame(
    ID = 1:5,
    setting = c("foo1", "foo2", "foo3", "foo4", "foo5"),
    file = paste0("foo_", c(1:5), ".xml"),
    experiment_id = data.table::data.table(
      DBI::dbReadTable(testcon, "experiments")
    )[name == "test", experiment_id]
  )
  .addScenToDB(testcon, scens)

  ## Check scenarios table
  expected <- data.frame(experiment_id = rep(1, 5), scenario_id = 1:5)
  actual <- DBI::dbReadTable(testcon, "scenarios")
  expect_equal(actual, expected)

  ## Check scenarios_metadata table
  expected <- data.frame(
    experiment_id = rep(1, 10),
    scenario_id = rep(c(1:5), 2),
    key_var = c(rep("setting", 5), rep("file", 5)),
    value = c(
      "foo1", "foo2", "foo3", "foo4", "foo5",
      paste0("foo_", c(1:5), ".xml")
    )
  )
  actual <- DBI::dbReadTable(testcon, "scenarios_metadata")
  expect_equal(actual, expected)

  ## Close connection
  DBI::dbDisconnect(testcon)
})

test_that(".addPlaceholdersToDB works", {
  testcon <- .createDB("test")

  foo1 <- rnorm(5)
  foo2 <- rnorm(5)
  places <- data.frame(
    ID = 1:5,
    experiment_id = data.table::data.table(
      DBI::dbReadTable(testcon, "experiments")
    )[name == "test", experiment_id],
    foo1 = foo1,
    foo2 = foo2
  )
  .addPlaceholdersToDB(testcon, places)

  expected <- data.frame(experiment_id = rep(1, 5), scenario_id = 1:5)
  actual <- DBI::dbReadTable(testcon, "scenarios")
  expect_equal(actual, expected)

  ## Check scenarios_metadata table
  expected <- data.frame(
    experiment_id = rep(1, 10),
    scenario_id = rep(c(1:5), 2),
    placeholder = c(rep("foo1", 5), rep("foo2", 5)),
    value = c(foo1, foo2)
  )
  actual <- DBI::dbReadTable(testcon, "placeholders")
  expect_equal(actual, expected)

  ## Close connection
  DBI::dbDisconnect(testcon)
})

test_that(".addResultsToDB works", {
  testcon <- .createDB("test")

  results <- readOutputFile(file.path(rootDir, "test.txt"))
  results <- data.table::data.table(experiment_id = 1, scenario_id = 2, results)

  .addResultsToDB(testcon, results)

  expected <- as.data.frame(results)
  actual <- DBI::dbReadTable(testcon, "results")
  expect_equal(actual, expected)

  ## Close connection
  DBI::dbDisconnect(testcon)
})

test_that("readResults works", {
  clearCache()
  ## Remove any remaining database
  if (file.exists(file.path(rootDir, "test.sqlite"))) {
    unlink(file.path(rootDir, "test.sqlite"))
  }

  ## Cache
  setupDirs("test", rootDir = rootDir, replace = TRUE)

  ## Input data

  ## Scenarios
  scenarios <- generateScenarios(
    data.frame(setting = c("foo1", "foo2", "foo3", "foo4", "foo5"))
  )
  storeScenarios(scenarios = scenarios, csv = FALSE)

  ## OM output
  testdata <- read.delim(
    text = "
1	1	0	71
1	2	0	57
1	3	0	151
", sep = "\t", header = FALSE
  )
  for (i in seq_len(5)) {
    write.table(
      testdata,
      file = file.path(
        getCache("outputsDir"),
        paste0("test_", i, "_out.txt")
      ), row.names = FALSE
    )
  }
  expected <- data.table::data.table(testdata)
  colnames(expected) <- c(
    "survey_date", "third_dimension", "measure", "value"
  )
  dates <- .xmlMonitoringTimeRegularSeq(
    "2000-01-01", "2000-03-20",
    daysFilter = 5, dateFilter = "monthly"
  )
  putCache(
    "surveyTimes",
    data.table::data.table(number = seq.int(nrow(dates)), dates)
  )

  syncCache(path = getCache("experimentDir"))

  readResults(
    expDir = getCache("experimentDir"), dbName = "test"
  )

  ## Replace
  expect_warning(
    readResults(
      expDir = getCache("experimentDir"), dbName = "test", replace = TRUE
    )
  )

  ## Test DB content
  testcon <- .createDB("test")
  expected <- data.frame(experiment_id = 1, name = "test")
  actual <- DBI::dbReadTable(testcon, "experiments")
  expect_equal(actual, expected)

  ## Check scenarios table
  expected <- data.frame(experiment_id = rep(1, 5), scenario_id = 1:5)
  actual <- DBI::dbReadTable(testcon, "scenarios")
  expect_equal(actual, expected)

  expected <- data.frame(
    experiment_id = rep(1, 10),
    scenario_id = rep(c(1:5), 2),
    key_var = c(rep("setting", 5), rep("file", 5)),
    value = c(
      "foo1", "foo2", "foo3", "foo4", "foo5",
      paste0("test_", c(1:5), ".xml")
    )
  )
  actual <- DBI::dbReadTable(testcon, "scenarios_metadata")
  expect_equal(actual, expected)

  results <- readOutputFile(file.path(rootDir, "test.txt"))
  results <- data.table::rbindlist(
    l = list(
      data.table::data.table(experiment_id = 1, scenario_id = 1, results),
      data.table::data.table(experiment_id = 1, scenario_id = 2, results),
      data.table::data.table(experiment_id = 1, scenario_id = 3, results),
      data.table::data.table(experiment_id = 1, scenario_id = 4, results),
      data.table::data.table(experiment_id = 1, scenario_id = 5, results)
    )
  )
  expected <- as.data.frame(results)
  actual <- DBI::dbReadTable(testcon, "results")
  expect_equal(actual, expected)

  ## Close connection
  DBI::dbDisconnect(conn = testcon)
})
