## Setup
testDir <- file.path(tempdir(), "test-cache", "cache")
dir.create(path = testDir, recursive = TRUE)


test_that("putCache works", {
  putCache(x = "foo", value = c(1, 2, 3))
  actual <- get(x = "foo", envir = openMalariaUtilities:::.pkgcache)[["value"]]
  expected <- c(1, 2, 3)

  expect_equal(actual, expected)
})

test_that("getCache works", {
  putCache(x = "foo", value = c(4, 5, 6))

  ## Retrieve value
  actual <- getCache(x = "foo", ret = "value")
  expected <- c(4, 5, 6)
  expect_equal(actual, expected)

  ## Retrieve timestamp
  actual <- getCache(x = "foo", ret = "timestamp")
  expected <- get(
    x = "foo", envir = openMalariaUtilities:::.pkgcache
  )[["timestamp"]]
  expect_equal(actual, expected)

  ## Retrieve both
  actual <- getCache(x = "foo", ret = "both")
  expected <- list(
    value = c(4, 5, 6),
    timestamp = get(
      x = "foo", envir = openMalariaUtilities:::.pkgcache
    )[["timestamp"]]
  )
  expect_equal(actual, expected)

  ## Check error
  expect_error(getCache(x = "foo", ret = "bar"))
})

test_that("clearCache works", {
  ## Have at least one object in cache.
  assign(
    x = "foo", value = c(1, 2, 3),
    envir = openMalariaUtilities:::.pkgcache
  )

  clearCache()

  actual <- ls(all.names = TRUE, envir = openMalariaUtilities:::.pkgcache)
  expected <- character(0)

  expect_equal(actual, expected)
})

test_that(".syncDisk works", {
  clearCache()

  putCache(x = "placeholders", value = c("foo"))
  putCache(x = "cacheDir", value = file.path(testDir))
  .syncDisk(file.path(getCache(x = "cacheDir")))

  actual <- file.exists(file.path(getCache(x = "cacheDir"), "cache.rds"))
  expected <- TRUE

  expect_equal(actual, expected)
})

test_that(".syncMem works", {
  clearCache()

  ## Env is empty
  expect_equal(
    ls(all.names = TRUE, envir = openMalariaUtilities:::.pkgcache),
    character(0)
  )

  ## Restore cache
  .syncMem(diskCache = readRDS(file = file.path(testDir, "cache.rds")))
  actual <- getCache(x = "placeholders")
  expected <- c("foo")

  expect_equal(actual, expected)
})

test_that(".syncBoth works", {
  clearCache()

  ## Env is empty
  expect_equal(
    ls(all.names = TRUE, envir = openMalariaUtilities:::.pkgcache),
    character(0)
  )

  ## Restore cache in memory
  tempCache <- .syncBoth(
    diskCache = readRDS(file = file.path(testDir, "cache.rds")),
    memCache = openMalariaUtilities:::.pkgcache
  )
  ## Assign the new cache to memory
  for (x in ls(all.names = TRUE, envir = tempCache)) {
    val <- get(x = as.character(x), envir = tempCache)
    assign(x = as.character(x), value = val, envir = .pkgcache)
  }
  actual <- getCache(x = "placeholders")
  expected <- c("foo")
  expect_equal(actual, expected)
})

test_that(".synchronizeCache works", {
  clearCache()

  ## Env is empty
  expect_equal(
    ls(all.names = TRUE, envir = openMalariaUtilities:::.pkgcache),
    character(0)
  )

  ## Remove cache on disk
  unlink(file.path(testDir, "cache.rds"))
  expect_equal(file.exists(file.path(testDir, "cache.rds")), FALSE)

  ## Write cache to disk
  putCache(x = "placeholders", value = c("foo"))
  putCache(x = "cacheDir", value = file.path(testDir))
  .synchronizeCache(path = testDir, direction = "disk")
  actual <- file.exists(file.path(getCache(x = "cacheDir"), "cache.rds"))
  expected <- TRUE
  expect_equal(actual, expected)


  ## Restore from disk
  clearCache()

  ## Env is empty
  expect_equal(
    ls(all.names = TRUE, envir = openMalariaUtilities:::.pkgcache),
    character(0)
  )

  ## Restore cache
  .synchronizeCache(path = testDir, direction = "memory")
  actual <- getCache(x = "placeholders")
  expected <- c("foo")
  expect_equal(actual, expected)


  ## Remove cache on disk
  unlink(file.path(testDir, "cache.rds"))
  expect_equal(file.exists(file.path(testDir, "cache.rds")), FALSE)

  ## Synchronize both caches
  .synchronizeCache(path = testDir, direction = "none")
  expect_equal(file.exists(file.path(testDir, "cache.rds")), TRUE)
})

test_that("loadExperiment works", {
  clearCache()
  putCache(x = "placeholders", value = c("foo", "bar", "baz"))
  .synchronizeCache(path = testDir, direction = "disk")
  clearCache()

  loadExperiment(file.path(tempdir(), "test-cache"))
  actual <- getCache(x = "placeholders")
  expected <- c("foo", "bar", "baz")

  expect_equal(actual, expected)
})

test_that("writeCache works", {
  clearCache()
  putCache(x = "placeholders", value = c("foo", "bar"))
  writeCache(path = file.path(tempdir(), "test-cache"))

  putCache(x = "placeholders", value = c("foo", "bar", "baz"))
  loadExperiment(file.path(tempdir(), "test-cache"))
  actual <- getCache(x = "placeholders")
  expected <- c("foo", "bar")

  expect_equal(actual, expected)
})

test_that("syncCache works", {
  clearCache()
  putCache(x = "placeholders", value = c("foo", "bar"))
  writeCache(path = file.path(tempdir(), "test-cache"))
  clearCache()
  putCache(x = "otherholders", value = c("baz"))

  syncCache(file.path(tempdir(), "test-cache"))
  actual <- getCache(x = "placeholders")
  expected <- c("foo", "bar")
  expect_equal(actual, expected)
  actual <- getCache(x = "otherholders")
  expected <- c("baz")
  expect_equal(actual, expected)
})

test_that("listCache works", {

  ## Clear cache
  clearCache()

  putCache(x = "foo", value = c(1, 2, 3))
  putCache(x = "bar", value = "A string")
  putCache(x = "baz", value = 42)
  putCache(x = "meppa", value = data.frame(a = c(1:5), b = letters[1:5]))

  expect_output(listCache(), "Name: bar")
  expect_output(listCache(), "Class: character")
  expect_output(listCache(), "Class: numeric")
  expect_output(listCache(), "A string")
  expect_output(listCache(), "42")
  expect_output(listCache(), "3 3 c")
})
