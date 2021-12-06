test_that("storeScenarios works", {
  scenarios <- data.frame(pop = c(1:10))
  full <- "foo"
  .omupkgcache$cacheDir <- file.path(tempdir(), "cache")
  storeScenarios(scenarios = scenarios, full = full)

  actual <- file.exists(file.path(.omupkgcache$cacheDir, "scens.RData"))
  expected <- TRUE

  expect_equal(actual, expected)
})

test_that(".scenariosRowSelect works (full range)", {
  scenarios <- data.frame(pop = c(1:10))
  actual <- seq_len(nrow(scenarios))
  expected <- .scenariosRowSelect(scenarios = scenarios)

  expect_equal(actual, expected)
})

test_that(".scenariosRowSelect works (custom range)", {
  scenarios <- data.frame(pop = c(1:10))
  actual <- 3:5
  expected <- .scenariosRowSelect(scenarios = scenarios, rowStart = 3, rowEnd = 5)

  expect_equal(actual, expected)
})
testthat::test_path("examples", "image.png")

test_that(".scenariosGenFiles works", {
  .omupkgcache$cacheDir <- file.path(tempdir(), "cache")
  .omupkgcache$scenariosDir <- file.path(tempdir(), "scenarios")
  .omupkgcache$placeholders <- "pop"
  scenarios <- data.frame(pop = c(1:10))
  full <- "foo"

  unlink(.omupkgcache$scenariosDir, recursive = TRUE)
  dir.create(.omupkgcache$scenariosDir)

  .scenariosGenFiles(scenarios = scenarios,
                     baseFile = testthat::test_path("ref", "exp_test_base.xml"),
                     range = seq_len(nrow(scenarios)), placeholders = c("pop"),
                     prefix = "exp_test")

  scen_files <- paste0("exp_test_", c(1:10), ".xml")
  actual <- file.exists(file.path(.omupkgcache$scenariosDir, scen_files))
  expected <- rep(TRUE, 10)
  
  expect_equal(actual, expected)
})

test_that(".scenariosGenFiles emits error", {
  .omupkgcache$cacheDir <- file.path(tempdir(), "cache")
  .omupkgcache$scenariosDir <- file.path(tempdir(), "scenarios")
  .omupkgcache$placeholders <- "pop"
  scenarios <- data.frame(pop = c(1:10))
  full <- "foo"

  unlink(.omupkgcache$scenariosDir, recursive = TRUE)
  dir.create(.omupkgcache$scenariosDir)

  expect_error(.scenariosGenFiles(scenarios = scenarios,
                                  baseFile = testthat::test_path("ref", "exp_test_base.xml"),
                                  range = seq_len(nrow(scenarios)), placeholders = c("foo"),
                                  prefix = "exp_test"),
               "variables are definded in the base xml file but not in the scenarios")
})

test_that(".scenariosGenFiles emits warning", {
  .omupkgcache$cacheDir <- file.path(tempdir(), "cache")
  .omupkgcache$scenariosDir <- file.path(tempdir(), "scenarios")
  .omupkgcache$placeholders <- "pop"
  scenarios <- data.frame(pop = c(1:10), foo = c(1:10))
  full <- "foo"

  unlink(.omupkgcache$scenariosDir, recursive = TRUE)
  dir.create(.omupkgcache$scenariosDir)

  expect_warning(.scenariosGenFiles(scenarios = scenarios,
                                  baseFile = testthat::test_path("ref", "exp_test_base.xml"),
                                  range = c(1:2), placeholders = c("pop", "foo"),
                                  prefix = "exp_test"),
               "variables are not used in the base xml file but definded in the scenarios")
})

test_that("generateScenarios works", {
  .omupkgcache$cacheDir <- file.path(tempdir(), "cache")
  .omupkgcache$scenariosDir <- file.path(tempdir(), "scenarios")
  .omupkgcache$placeholders <- "pop"
  scenarios <- data.frame(pop = c(1:10))
  full <- "foo"

  unlink(.omupkgcache$scenariosDir, recursive = TRUE)
  dir.create(.omupkgcache$scenariosDir)

  generateScenarios(scenarios = scenarios, full = full,
                    baseFile = testthat::test_path("ref", "exp_test_base.xml"),
                    prefix = "exp_test")

  scen_files <- paste0("exp_test_", c(1:10), ".xml")
  actual <- file.exists(file.path(.omupkgcache$scenariosDir, scen_files))
  expected <- rep(TRUE, 10)

  expect_equal(actual, expected)
})
