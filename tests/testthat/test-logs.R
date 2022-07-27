## Setup
clearCache()
rootDir <- file.path(tempdir(), "test-logs")
setupDirs("test", rootDir = rootDir, replace = TRUE)

test_that("deleteLogs works", {
  ## Create dummy logfiles
  dummies <- c()
  for (f in paste0("foo", c(1:10), ".txt")) {
    for (d in c(
      file.path(getCache(x = "logsDir"), "scenarios"),
      file.path(getCache(x = "logsDir"), "simulation")
    )) {
      dummies <- c(file.path(d, f), dummies)
    }
  }
  for (f in dummies) {
    cat("bar\n", file = f)
  }

  ## Check that files exist
  actual <- file.exists(dummies)
  expected <- rep(TRUE, 20)

  expect_equal(actual, expected)

  ## Delete files
  deleteLogs()

  ## Check that they do not exist anymore
  actual <- file.exists(dummies)
  expected <- rep(FALSE, 20)

  expect_equal(actual, expected)
})

test_that("cleanLogs works", {
  ## Create dummy logfiles
  dummies <- c()
  for (f in paste0("foo", c(1:10), ".txt")) {
    for (d in c(
      file.path(getCache(x = "logsDir"), "scenarios"),
      file.path(getCache(x = "logsDir"), "simulation")
    )) {
      dummies <- c(file.path(d, f), dummies)
    }
  }
  for (f in dummies) {
    cat("bar\n", file = f)
  }

  ## Check that files exist
  actual <- file.exists(dummies)
  expected <- rep(TRUE, 20)

  expect_equal(actual, expected)

  ## Cleanup files, aggregate
  cleanLogs(compress = FALSE)

  ## Check that the txt files do not exist anymore
  actual <- file.exists(dummies)
  expected <- rep(FALSE, 20)

  expect_equal(actual, expected)

  ## Check that the single log files exist
  actual <- file.exists(
    list.files(
      path = getCache(x = "logsDir"),
      pattern = ".*\\.txt$", recursive = TRUE, full.names = TRUE
    )
  )
  expected <- rep(TRUE, 2)

  expect_equal(actual, expected)


  ## Reset
  setupDirs("test", rootDir = rootDir, replace = TRUE)
  ## Create dummy logfiles
  dummies <- c()
  for (f in paste0("foo", c(1:10), ".txt")) {
    for (d in c(
      file.path(getCache(x = "logsDir"), "scenarios"),
      file.path(getCache(x = "logsDir"), "simulation")
    )) {
      dummies <- c(file.path(d, f), dummies)
    }
  }
  for (f in dummies) {
    cat("bar\n", file = f)
  }

  ## Cleanup files, aggregate
  cleanLogs(compress = TRUE, aggregate = FALSE)

  ## Check that the zip files exist
  actual <- file.exists(
    list.files(
      path = getCache(x = "logsDir"),
      pattern = ".*\\.zip$", recursive = TRUE, full.names = TRUE
    )
  )
  expected <- rep(TRUE, 2)

  expect_equal(actual, expected)


  ## Reset
  setupDirs("test", rootDir = rootDir, replace = TRUE)
  ## Create dummy logfiles
  dummies <- c()
  for (f in paste0("foo", c(1:10), ".txt")) {
    for (d in c(
      file.path(getCache(x = "logsDir"), "scenarios"),
      file.path(getCache(x = "logsDir"), "simulation")
    )) {
      dummies <- c(file.path(d, f), dummies)
    }
  }
  for (f in dummies) {
    cat("bar\n", file = f)
  }

  ## Cleanup files
  cleanLogs()

  ## Check that the zip files exist
  actual <- file.exists(
    list.files(
      path = getCache(x = "logsDir"),
      pattern = ".*\\.zip$", recursive = TRUE, full.names = TRUE
    )
  )
  expected <- rep(TRUE, 2)

  expect_equal(actual, expected)
})
