## FIXME We should have tests for that; Need to figure out how files are stored
##       during Github actions.

test_that(".storeCache works", {
  omuCache <- hash::hash()
  omuCache$placeholders <- c("foo")
  omuCache$cacheDir <- file.path(tempdir(), "cache")
  .storeCache()

  actual <- file.exists(file.path(omuCache$cacheDir, "cache.RData"))
  expected <- TRUE
  
  expect_equal(actual, expected)
})

test_that(".readCache works", {
  .readCache(tempdir())
  actual <- exists("omuCache")
  expected <- TRUE

  expect_equal(actual, expected)
})

test_that("loadExperiment works", {
  loadExperiment(tempdir())
  actual <- exists("omuCache")
  expected <- TRUE

  expect_equal(actual, expected)
})
