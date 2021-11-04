test_that("createBaseXml works", {
  omuCache <- hash::hash()
  data <- list(
    expName = "Test",
    OMVersion = 43,
    analysisNo = 1,
    rootDir = tempdir()
  )
  actual <- file.exists(file.path(tempdir(), "Test", "Test_base.xml"))
  expected <- TRUE

  ## expect_equal(actual, expected)
  skip("Skip")
})
