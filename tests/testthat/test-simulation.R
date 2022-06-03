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
  expect_output(runSimulations(scenarios = scenarios, dryRun = TRUE))

  ## Check if log files have been written
  ## Normal log files
  expect_gt(
    length(
      list.files(
        file.path(
          getCache(x = "logsDir"), "simulation"
        ),
        pattern = ".*[0-9]+\\.log"
      )
    ),
    0
  )
  ## Normal log files
  expect_gt(
    length(
      list.files(
        file.path(
          getCache(x = "logsDir"), "simulation"
        ),
        pattern = ".*[0-9]+_error\\.log"
      )
    ),
    0
  )

  ## Cluster run
  file.remove(
    list.files(
      file.path(getCache(x = "logsDir"), "simulation"),
      pattern = ".*\\.log"
    )
  )

  runSimulations(scenarios = scenarios, dryRun = TRUE, ncores = 2)

  ## Check if log files have been written
  ## Normal log files
  expect_gt(
    length(
      list.files(
        file.path(
          getCache(x = "logsDir"), "simulation"
        ),
        pattern = ".*[0-9]+\\.log"
      )
    ),
    0
  )
  ## Normal log files
  expect_gt(
    length(
      list.files(
        file.path(
          getCache(x = "logsDir"), "simulation"
        ),
        pattern = ".*[0-9]+_error\\.log"
      )
    ),
    0
  )
})
