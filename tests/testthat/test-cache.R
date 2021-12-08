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
