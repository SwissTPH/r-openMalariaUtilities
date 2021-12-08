test_that(".storeCache works", {
  .omupkgcache$placeholders <- c("foo")
  .omupkgcache$cacheDir <- file.path(tempdir(), "cache")
  .storeCache()

  actual <- file.exists(file.path(.omupkgcache$cacheDir, "cache.rds"))
  expected <- TRUE
  
  expect_equal(actual, expected)
})

test_that(".readCache works", {
  .readCache(tempdir())
  actual <- exists(".omupkgcache")
  expected <- TRUE

  expect_equal(actual, expected)
})

test_that("loadExperiment works", {
  loadExperiment(tempdir())
  actual <- exists(".omupkgcache")
  expected <- TRUE

  expect_equal(actual, expected)
})
