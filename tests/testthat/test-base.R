test_that("createBaseXml works", {
  data <- list(
    expName = "Test",
    OMVersion = 43L,
    analysisNo = 1L,
    rootDir = tempdir()
  )
  createBaseXml(data = data, replace = TRUE)
  actual <- file.exists(file.path(tempdir(), "Test", "Test_base.xml"))
  expected <- TRUE

  expect_equal(actual, expected)
})
