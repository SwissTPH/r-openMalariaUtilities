## Setup
clearCache()
rootDir <- file.path(tempdir(), "test-validation")
setupDirs("test", rootDir = rootDir, replace = TRUE)
putCache(x = "OMVersion", value = 44L)
setupOM()

test_that("validateXML works", {
  xmlFile <- testthat::test_path("ref", "exp_test_base.xml")
  scenarios <- data.frame(pop = c(1:10))

  actual <- validateXML(xmlfile = xmlFile, scenarios = scenarios)
  expected <- TRUE

  expect_equal(actual, expected)

  ## Test that error is caught
  ## Modify file
  f <- processFile(f = xmlFile, trim = FALSE, rmdups = FALSE)
  f <- gsub(pattern = "@pop@", replacement = "foo", x = f)
  writeLines(f, con = file.path(rootDir, "exp_test_base.xml"))
  expect_warning(
    validateXML(
      xmlfile = file.path(rootDir, "exp_test_base.xml"),
      scenarios = scenarios
    )
  )

  ## Test that error is caught is scenarios
  ## Modify file
  scenarios$pop <- as.character(scenarios$pop)

  expect_warning(
    validateXML(xmlfile = xmlFile, scenarios = scenarios)
  )
})
