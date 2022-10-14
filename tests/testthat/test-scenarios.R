## Setup
clearCache()
rootDir <- file.path(tempdir(), "test-scenarios")
setupDirs("test", rootDir = rootDir, replace = TRUE)

test_that(".scenariosRowSelect works", {
  ## Full range
  scenarios <- data.frame(pop = c(1:10))
  actual <- seq_len(nrow(scenarios))
  expected <- .scenariosRowSelect(scenarios = scenarios)

  expect_equal(actual, expected)

  ## Custom range
  scenarios <- data.frame(pop = c(1:10))
  actual <- 3:5
  expected <- .scenariosRowSelect(
    scenarios = scenarios, rowStart = 3, rowEnd = 5
  )

  expect_equal(actual, expected)
})

test_that(".scenariosGenFiles works", {
  putCache("placeholders", "pop")
  scenarios <- data.frame(pop = c(1:10))

  unlink(
    getCache(x = "scenariosDir"),
    recursive = TRUE
  )
  dir.create(getCache(x = "scenariosDir"))

  ## Single core
  .scenariosGenFiles(
    scenarios = scenarios,
    baseFile = testthat::test_path("ref", "exp_test_base.xml"),
    range = seq_len(nrow(scenarios)), placeholders = c("pop"),
    prefix = "exp_test"
  )

  scen_files <- paste0("exp_test_", c(1:10), ".xml")
  actual <- file.exists(file.path(getCache("scenariosDir", ), scen_files))
  expected <- rep(TRUE, 10)

  expect_equal(actual, expected)

  ## Multi core
  unlink(
    getCache(x = "scenariosDir"),
    recursive = TRUE
  )
  dir.create(getCache(x = "scenariosDir"))

  .scenariosGenFiles(
    scenarios = scenarios,
    baseFile = testthat::test_path("ref", "exp_test_base.xml"),
    range = seq_len(nrow(scenarios)), placeholders = c("pop"),
    prefix = "exp_test", ncores = 2
  )

  scen_files <- paste0("exp_test_", c(1:10), ".xml")
  actual <- file.exists(file.path(getCache("scenariosDir", ), scen_files))
  expected <- rep(TRUE, 10)

  expect_equal(actual, expected)

  ## Emits error
  unlink(
    getCache(x = "scenariosDir"),
    recursive = TRUE
  )
  dir.create(getCache(x = "scenariosDir"))

  expect_error(
    .scenariosGenFiles(
      scenarios = scenarios,
      baseFile = testthat::test_path("ref", "exp_test_base.xml"),
      range = seq_len(nrow(scenarios)), placeholders = c("pop", "foo"),
      prefix = "exp_test"
    ),
    "variables are definded in the base xml file but not in the scenarios"
  )
})

test_that("finalizeScenarios works", {
  putCache("experimentName", "test")

  scenarios <- data.frame(
    futITNcov = c(.65),
    futIRScov = c(0, .8),
    EIR = c(5, 25),
    setting = c("alpha"),
    pop = c(1:10),
    seed = 1
  )

  unlink(getCache(x = "scenariosDir"),
    recursive = TRUE
  )
  dir.create(getCache(x = "scenariosDir"))

  actual <- finalizeScenarios(x = scenarios)
  expected <- data.frame(
    ID = 1:10,
    futITNcov = c(.65),
    futIRScov = c(0, .8),
    EIR = c(5, 25),
    setting = c("alpha"),
    pop = c(1:10),
    seed = 1,
    check.names = FALSE,
    file = paste0("test_", c(1:10), ".xml")
  )

  expect_equal(actual, expected)

  ## Error if ID or file column exists
  expect_warning(
    finalizeScenarios(x = data.frame(
      scenarios,
      ID = seq_len(nrow(scenarios))
    ))
  )
})

test_that("setupScenarios works", {
  putCache("cacheDir", file.path(tempdir(), "cache"))
  putCache("scenariosDir", file.path(tempdir(), "scenarios"))
  putCache("placeholders", "pop")
  scenarios <- data.frame(
    pop = c(1:10)
  )

  unlink(getCache(x = "scenariosDir"),
    recursive = TRUE
  )
  dir.create(getCache(x = "scenariosDir"))

  setupScenarios(
    scenarios = scenarios,
    baseFile = testthat::test_path("ref", "exp_test_base.xml"),
    prefix = "exp_test"
  )

  scen_files <- paste0("exp_test_", c(1:10), ".xml")
  actual <- file.exists(file.path(getCache(
    x = "scenariosDir"
  ), scen_files))
  expected <- rep(TRUE, 10)

  expect_equal(actual, expected)
})

test_that("storing and restoring scenarios works", {
  scenarios <- data.frame(
    pop = c(1:10)
  )
  putCache("experimentDir", tempdir())
  putCache("cacheDir", file.path(tempdir(), "cache"))
  dir.create(getCache(x = "cacheDir"))
  storeScenarios(scenarios = scenarios)

  ## Check for rds file
  actual <- file.exists(file.path(getCache(
    x = "cacheDir"
  ), "scenarios.rds"))
  expected <- TRUE
  expect_equal(actual, expected)

  ## Check for csv file
  actual <- file.exists(file.path(getCache(
    x = "experimentDir"
  ), "scenarios.csv"))
  expected <- TRUE
  expect_equal(actual, expected)

  ## Restore scenarios
  expected <- scenarios
  rm("scenarios")
  actual <- readScenarios()
  expect_equal(actual, expected)
})
