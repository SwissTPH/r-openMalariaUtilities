test_that(".storeCache works", {
  assign(
    x = "placeholders", value = c("foo"),
    envir = openMalariaUtilities:::.pkgcache
  )
  assign(
    x = "cacheDir", value = file.path(tempdir(), "cache"),
    envir = openMalariaUtilities:::.pkgcache
  )
  .storeCache()

  actual <- file.exists(file.path(get(
    x = "cacheDir",
    envir = openMalariaUtilities:::.pkgcache
  ), "cache.rds"))
  expected <- TRUE

  expect_equal(actual, expected)
})

test_that(".readCache works", {
  assign(
    x = "placeholders", value = c("foo", "bar"),
    envir = openMalariaUtilities:::.pkgcache
  )
  .storeCache()
  .readCache(tempdir())
  actual <- get(x = "placeholders", envir = openMalariaUtilities:::.pkgcache)
  expected <- c("foo", "bar")

  expect_equal(actual, expected)
})

test_that("loadExperiment works", {
  assign(
    x = "placeholders", value = c("foo", "bar", "baz"),
    envir = openMalariaUtilities:::.pkgcache
  )
  .storeCache()

  loadExperiment(tempdir())
  actual <- get(x = "placeholders", envir = openMalariaUtilities:::.pkgcache)
  expected <- c("foo", "bar", "baz")

  expect_equal(actual, expected)
})

test_that("putCache works", {
  putCache(x = "foo", value = c(1, 2, 3))
  actual <- get(x = "foo", envir = openMalariaUtilities:::.pkgcache)
  expected <- c(1, 2, 3)

  expect_equal(actual, expected)
})

test_that("getCache works", {
  assign(
    x = "foo", value = c(1, 2, 3),
    envir = openMalariaUtilities:::.pkgcache
  )

  actual <- getCache(x = "foo")
  expected <- c(1, 2, 3)

  expect_equal(actual, expected)
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

test_that("listCache works", {
  ## Clear cache
  clearCache()

  assign(
    x = "foo", value = c(1, 2, 3),
    envir = openMalariaUtilities:::.pkgcache
  )

  actual <- listCache()
  expected <- c("foo")

  expect_equal(actual, expected)
})
