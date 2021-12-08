test_that("runScenarios throws an error if cmd not found", {
  assign("scenariosDir", file.path(tempdir(), "scenarios"),
    envir = openMalariaUtilities:::.pkgcache
  )
  unlink(get(x = "scenariosDir", envir = openMalariaUtilities:::.pkgcache),
    recursive = TRUE
  )
  dir.create(get(x = "scenariosDir", envir = openMalariaUtilities:::.pkgcache))

  expect_error(
    runScenarios(cmd = "NotOpenMalaria"),
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
  assign("cacheDir", file.path(tempdir(), "cache"),
    envir = openMalariaUtilities:::.pkgcache
  )
  assign("scenariosDir", file.path(tempdir(), "scenarios"),
    envir = openMalariaUtilities:::.pkgcache
  )
  assign("placeholders", "pop",
    envir = openMalariaUtilities:::.pkgcache
  )

  scenarios <- data.frame(pop = c(1:10))
  scen <- scenarios
  full <- "foo"

  unlink(get(x = "scenariosDir", envir = openMalariaUtilities:::.pkgcache),
    recursive = TRUE
  )
  dir.create(get(x = "scenariosDir", envir = openMalariaUtilities:::.pkgcache))

  generateScenarios(
    scenarios = scenarios, full = full,
    baseFile = testthat::test_path("ref", "exp_test_base.xml"),
    prefix = "exp_test"
  )

  expect_output(runScenarios(cmd = "R", dryRun = TRUE))
})
