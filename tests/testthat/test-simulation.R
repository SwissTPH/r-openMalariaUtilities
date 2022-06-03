test_that("runSimulations works", {
  clearCache()
  unlink(file.path(tempdir(), "test-simulation"), recursive = TRUE)
  setupDirs(
    experimentName = "test-simulation", rootDir = tempdir(), replace = TRUE
  )

  scenarios <- generateScenarios(data.frame(foo = rnorm(5), bar = rnorm(5)))

  ## Throws an error if cmd not found
  expect_error(
    runSimulations(scenarios = scenarios, cmd = "NotOpenMalaria"),
    "openMalaria could not be found"
  )

  ## Normal run with console output
  ## Make sure logfiles don't exist beforehand
  removeFiles <- list.files(
    file.path(getCache(x = "logsDir"), "simulation"),
    pattern = ".*\\.log", full.names = TRUE
  )
  if (any(file.exists(removeFiles))) {
    file.remove(removeFiles)
  }

  expect_output(runSimulations(scenarios = scenarios, dryRun = TRUE))

  ## Check if log files have been written
  ## Expected log files
  logfiles <- paste0("test-simulation_", c(1:5), ".log")
  errlogfiles <- paste0("test-simulation_", c(1:5), "_error.log")


  expect_equal(
    list.files(
      file.path(
        getCache(x = "logsDir"), "simulation"
      ),
      pattern = ".*[0-9]+\\.log"
    ),
    logfiles
  )
  ## Normal log files
  expect_equal(
    list.files(
      file.path(
        getCache(x = "logsDir"), "simulation"
      ),
      pattern = ".*[0-9]+_error\\.log"
    ),
    errlogfiles
  )

  ## Cluster run
  ## Make sure they don't exist beforehand
  removeFiles <- list.files(
    file.path(getCache(x = "logsDir"), "simulation"),
    pattern = ".*\\.log", full.names = TRUE
  )
  if (any(file.exists(removeFiles))) {
    file.remove(removeFiles)
  }

  runSimulations(scenarios = scenarios, dryRun = TRUE, ncores = 2)

  ## Check if log files have been written
  ## Normal log files
  expect_equal(
    list.files(
      file.path(
        getCache(x = "logsDir"), "simulation"
      ),
      pattern = ".*[0-9]+\\.log"
    ),
    logfiles
  )
  ## Normal log files
  expect_equal(
    list.files(
      file.path(
        getCache(x = "logsDir"), "simulation"
      ),
      pattern = ".*[0-9]+_error\\.log"
    ),
    errlogfiles
  )
})
