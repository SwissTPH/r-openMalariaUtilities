## Setup
testDir <- file.path(tempdir(), "test-base")
dir.create(path = testDir, recursive = TRUE)


test_that(".thirdDimensionGen works", {
  clearCache()

  ## Test only age groups
  putCache("mon_ageGroups", list(lowerbound = 0, upperbounds = c(1, 2)))

  expected <- data.table::data.table(
    number = c(1:2, 0), id = c("0-1", "1-2", "none"))
  actual <- .thirdDimensionGen()

  expect_equal(actual, expected)

  ## Test with cohorts
  invisible(monitoringCohortsGen(c("A", "B")))
  expected <- data.table::data.table(
    number = c(1001, 1002, 2001, 2002, 3001, 3002, 1, 2, 0),
    id = c("A:0-1", "A:1-2", "B:0-1", "B:1-2", "AB:0-1", "AB:1-2", "0-1", "1-2", "none")
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
    number = c(1001, 1002, 2001, 2002, 3001, 3002, 1, 2, 0),
    id = c("A:0-1", "A:1-2", "B:0-1", "B:1-2", "AB:0-1", "AB:1-2", "0-1", "1-2", "none")
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

  expect_setequal(
    found,
    c("autoRegressionParameters.csv", "densities.csv", "scenario_44.xsd")
  )
})

test_that("setupOM copies the resources for each supported schema", {
  for (version in c(44L, 49L)) {
    resourceDir <- tempfile(paste0("setup-om-", version, "-"))
    dir.create(resourceDir)
    setupOM(version = version, dir = resourceDir)

    resources <- c(
      "autoRegressionParameters.csv", "densities.csv",
      paste0("scenario_", version, ".xsd")
    )
    expect_setequal(list.files(resourceDir), resources)
    for (resource in resources) {
      expect_identical(
        unname(tools::md5sum(file.path(resourceDir, resource))),
        unname(tools::md5sum(system.file(
          "extdata", resource, package = "openMalariaUtilities"
        )))
      )
    }

    schema <- xml2::read_xml(file.path(resourceDir, resources[3]))
    expect_identical(
      xml2::xml_attr(schema, "targetNamespace"),
      paste0("http://openmalaria.org/schema/scenario_", version)
    )

    ## Repeated setup preserves files already present in the experiment.
    sentinel <- file.path(resourceDir, "densities.csv")
    writeLines("existing experiment resource", sentinel)
    expect_message(setupOM(version = version, dir = resourceDir), "skipping")
    expect_identical(readLines(sentinel), "existing experiment resource")
  }
})
