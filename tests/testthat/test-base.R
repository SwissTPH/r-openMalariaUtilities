## Setup
testDir <- file.path(tempdir(), "test-base")
dir.create(path = testDir, recursive = TRUE)


test_that(".thirdDimensionGen works", {
  clearCache()

  ## Test only age groups
  putCache("mon_ageGroups", list(lowerbound = 0, upperbounds = c(1, 2)))

  expected <- data.table::data.table(number = 1:2, id = c("0-1", "1-2"))
  actual <- .thirdDimensionGen()

  expect_equal(actual, expected)

  ## Test with cohorts
  invisible(monitoringCohortsGen(c("A", "B")))
  expected <- data.table::data.table(
    number = c(1001, 1002, 2001, 2002, 3001, 3002, 1, 2),
    id = c("A:0-1", "A:1-2", "B:0-1", "B:1-2", "AB:0-1", "AB:1-2", "0-1", "1-2")
  )
  actual <- .thirdDimensionGen()

  expect_equal(actual, expected)
})

test_that("createBaseXml works", {
  clearCache()

  putCache("mon_ageGroups", list(lowerbound = 0, upperbounds = c(1, 2)))
  invisible(monitoringCohortsGen(c("A", "B")))

  data <- list(
    expName = "Test",
    OMVersion = 43L,
    analysisNo = 1L,
    rootDir = testDir
  )

  ## Test graceful error
  expect_error(
    createBaseXml(data = data, replace = TRUE),
    "Experiment directory could not be found"
  )

  ## Test XML file creation
  setupDirs(experimentName = "Test", rootDir = testDir, replace = TRUE)
  options(askYesNo = TRUE)
  createBaseXml(data = data)
  options(askYesNo = NULL)
  toggleDebug()
  expect_output(createBaseXml(data = data, replace = TRUE))
  toggleDebug()
  expect_error(createBaseXml(data = data, replace = FALSE))

  actual <- file.exists(file.path(testDir, "Test", "Test_base.xml"))
  expected <- TRUE

  expect_equal(actual, expected)

  expected <- data.table::data.table(
    number = c(1001, 1002, 2001, 2002, 3001, 3002, 1, 2),
    id = c("A:0-1", "A:1-2", "B:0-1", "B:1-2", "AB:0-1", "AB:1-2", "0-1", "1-2")
  )

  expect_equal(getCache("thirdDimension"), expected)
})

test_that("setupOM works", {
  clearCache()

  ## Test graceful error
  expect_error(
    setupOM(),
    "Experiment directory could not be found"
  )

  ## Test unsupported version
  expect_error(setupOM(version = 0))

  ## Test file download
  setupDirs(experimentName = "Test", rootDir = testDir, replace = TRUE)
  setupOM(dir = testDir)

  found <- list.files(testDir, pattern = ".csv|.xsd")

  actual <- all(grepl("autoRegressionParameters|densities|scenario", found))
  expected <- TRUE

  expect_equal(actual, expected)
})
