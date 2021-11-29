test_that("runScenarios throws an error if cmd not found", {
  expect_error(
    runScenarios(scenariosDir = "./"),
    "openMalaria could not be found"
  )
})

test_that("runScenarios throws an error if scenarios do not exist", {
  expect_error(
    ## R should be installed
    runScenarios(scenariosDir = "./", cmd = "R"),
    "does not exist"
  )
})

test_that("runScenarios works (dry run)", {
  ## Generate scenarios
  .omupkgcache$cacheDir <- file.path(tempdir(), "cache")
  .omupkgcache$scenariosDir <- file.path(tempdir(), "scenarios")
  .omupkgcache$placeholders <- "pop"
  scenarios <- data.frame(pop = c(1:10))
  scen <- scenarios
  full <- "foo"

  unlink(.omupkgcache$scenariosDir, recursive = TRUE)
  dir.create(.omupkgcache$scenariosDir)

  generateScenarios(scenarios = scenarios, full = full,
                    baseFile = testthat::test_path("ref", "exp_test_base.xml"),
                    prefix = "exp_test")

  expect_output(runScenarios(cmd = "R", dryRun = TRUE))
})
