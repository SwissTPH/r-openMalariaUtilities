## Setup
testDir <- file.path(tempdir(), "test-base")
dir.create(path = testDir, recursive = TRUE)

test_that("createBaseXml works", {
  clearCache()

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
