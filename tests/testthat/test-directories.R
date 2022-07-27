## Setup
testDir <- file.path(tempdir(), "test-directories")
dir.create(path = testDir, recursive = TRUE)

test_that(".useDir works", {
  testPath <- .useDir(file.path(testDir, "level_1", "level_2", "level_3"))
  expect_equal(dir.exists(testPath), TRUE)
})

test_that(".createFolders works", {
  .createFolders(
    experimentName = "Test",
    rootDir = testDir,
    replace = TRUE
  )

  actual <- all.equal(
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    c(
      file.exists(getCache(x = "rootDir")),
      file.exists(getCache(x = "cacheDir")),
      file.exists(getCache(x = "experimentDir")),
      file.exists(getCache(x = "scenariosDir")),
      file.exists(getCache(x = "logsDir")),
      file.exists(getCache(x = "outputsDir"))
    )
  )
  expected <- TRUE
  expect_equal(actual, expected)
})

test_that("setupDirs works", {
  setupDirs(
    experimentName = "Test2",
    rootDir = testDir,
    replace = TRUE
  )

  actual <- all.equal(
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    c(
      file.exists(getCache(x = "rootDir")),
      file.exists(getCache(x = "cacheDir")),
      file.exists(getCache(x = "experimentDir")),
      file.exists(getCache(x = "scenariosDir")),
      file.exists(getCache(x = "logsDir")),
      file.exists(getCache(x = "outputsDir"))
    )
  )
  expected <- TRUE
  expect_equal(actual, expected)
})
