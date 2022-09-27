## Setup
clearCache()
unlink(file.path(tempdir(), "test-utils"), recursive = TRUE)
rootDir <- .useDir(file.path(tempdir(), "test-utils"))


test_that("splitSeq works (no rest)", {
  actual <- splitSeq(1:20, 5)
  expected <- list(
    "1" = 1:5,
    "2" = 6:10,
    "3" = 11:15,
    "4" = 16:20
  )
  expect_equal(actual, expected)

  ## With rest
  actual <- splitSeq(1:21, 5)
  expected <- list(
    "1" = 1:5,
    "2" = 6:10,
    "3" = 11:15,
    "4" = 16:20,
    "5" = 21
  )
  expect_equal(actual, expected)

  ## Chunk size larger than length of x
  actual <- splitSeq(1:10, 15)
  expected <- list(
    "1" = 1:10
  )
  expect_equal(actual, expected)
})

test_that("processFile works", {
  tmpPath <- file.path(rootDir, "temp.txt")
  ## Create test file
  fcontent <- capture.output(cat("
foo

bar
bar
  baz
"))
  cat(fcontent, sep = "\n", file = tmpPath)

  ## Keep original
  actual <- processFile(f = tmpPath, trim = FALSE, rmdups = FALSE)
  expected <- fcontent
  expect_equal(actual, expected)

  ## Trim whitespace
  actual <- processFile(f = tmpPath, trim = TRUE, rmdups = FALSE)
  expected <- trimws(fcontent)
  expect_equal(actual, expected)

  ## Remove duplicates
  actual <- processFile(f = tmpPath, trim = FALSE, rmdups = TRUE)
  expected <- unique(fcontent)
  expected <- expected[expected != ""]
  expect_equal(actual, expected)

  ## Remove duplicates and trim whitespace
  actual <- processFile(f = tmpPath)
  expected <- unique(trimws(fcontent))
  expected <- expected[expected != ""]
  expect_equal(actual, expected)
})

test_that(".indexList works", {
  testList <- list(l1 = 2, l2 = list(l3 = 2))

  expected <- list(
    "2" = list(
      index = 2,
      name = "l2"
    ),
    "1" = list(
      index = 1,
      name = "l1",
      value = 2
    ),
    "2.1" = list(
      index = c(2, 1),
      name = "l3",
      value = 2
    )
  )

  expect_equal(.indexList(testList), expected)
})

test_that(".getIndex works", {
  testList <- .indexList(list(l1 = 2, l2 = list(l3 = 2)))

  expected <- list(
    "2.1" = list(
      index = c(2, 1),
      name = "l3",
      value = 2
    )
  )

  expect_equal(.getIndex(testList, name = "l3", value = "2"), expected)
})

test_that("extractList works", {
  testList <- list(l1 = 2, l2 = list(l3 = 2))

  expected <- list(
    "2" = list(
      l3 = 2
    )
  )

  expect_equal(extractList(testList, name = "l3", value = "2"), expected)

  ## Only the index
  expected <- list("2" = c(2, 1))

  expect_equal(
    extractList(testList, name = "l3", value = "2", onlyIndex = TRUE), expected
  )
})

test_that(".compressFiles works", {
  ## Clean
  for (f in list.files(rootDir, full.names = TRUE)) {
    file.remove(f)
  }

  ## Create dummy files
  dummies <- c()
  for (f in paste0("foo", c(1:10), ".txt")) {
    dummies <- c(file.path(rootDir, f), dummies)
  }
  for (f in paste0("foo", c(1:10), ".xml")) {
    dummies <- c(file.path(rootDir, f), dummies)
  }
  for (f in paste0("foo", c(1:10), ".R")) {
    dummies <- c(file.path(rootDir, f), dummies)
  }
  for (f in dummies) {
    cat("bar\n", file = f)
  }

  ## Check that files exist
  actual <- file.exists(dummies)
  expected <- rep(TRUE, 30)

  ## Compress selected pattern
  .compressFiles(dir = rootDir, pattern = ".*\\.R$", remove = TRUE)
  ## Check that zip file and not selected pattern exist
  actual <- file.exists(
    setdiff(
      list.files(path = rootDir, full.names = TRUE),
      list.dirs(path = rootDir, recursive = FALSE, full.names = TRUE)
    )
  )
  expected <- rep(TRUE, 21)

  expect_equal(actual, expected)

  ## Clean
  for (f in list.files(rootDir, full.names = TRUE)) {
    file.remove(f)
  }

  ## Create dummy files
  dummies <- c()
  for (f in paste0("foo", c(1:10), ".txt")) {
    dummies <- c(file.path(rootDir, f), dummies)
  }
  for (f in paste0("foo", c(1:10), ".xml")) {
    dummies <- c(file.path(rootDir, f), dummies)
  }
  for (f in paste0("foo", c(1:10), ".R")) {
    dummies <- c(file.path(rootDir, f), dummies)
  }
  for (f in dummies) {
    cat("bar\n", file = f)
  }

  ## Compress everything
  .compressFiles(dir = rootDir, remove = TRUE)
  ## Check that zip file and not selected pattern exist
  actual <- file.exists(
    setdiff(
      list.files(path = rootDir, full.names = TRUE),
      list.dirs(path = rootDir, recursive = FALSE, full.names = TRUE)
    )
  )
  expected <- TRUE

  expect_equal(actual, expected)
})


test_that("cleanupExperiment works", {
  setupDirs("test", rootDir = rootDir, replace = TRUE)

  ## Create dummy files
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
  dummies <- c()
  for (f in paste0("foo", c(1:10), ".xml")) {
    dummies <- c(file.path(getCache(x = "scenariosDir"), f), dummies)
  }
  for (f in dummies) {
    cat("bar\n", file = f)
  }
  dummies <- c()
  for (f in paste0("foo", c(1:10), ".txt")) {
    dummies <- c(file.path(getCache(x = "outputsDir"), f), dummies)
  }
  for (f in dummies) {
    cat("bar\n", file = f)
  }

  ## Clean everything
  cleanupExperiment()

  ## Check that zip files exist
  actual <- file.exists(
    list.files(path = getCache("experimentDir"), pattern = ".*\\.zip$", recursive = TRUE, full.names = TRUE)
  )
  expected <- rep(TRUE, 4)

  expect_equal(actual, expected)
})

test_that("expInfo works", {

  ## Clear cache
  clearCache()

  putCache(x = "OMVersion", value = 42)

  ## Full output
  expect_output(expInfo(what = "om_version"), "42")

  ## No printing
  expected <- list(om_version = 42)

  expect_equal(expInfo(what = "om_version", print = FALSE), expected)
})
