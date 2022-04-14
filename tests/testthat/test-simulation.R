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

  scenarios <- data.frame(
    futITNcov = c(.65),
    futIRScov = c(0, .8),
    EIR = c(5, 25),
    setting = c("alpha"),
    pop = c(1:10),
    seed = 1
  )
  scens <- scenarios
  full <- .create_test_full()

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
