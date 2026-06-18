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

  ## Test cohort recruitment and deployment XML pattern
  f <- processFile(f = xmlFile, trim = FALSE, rmdups = FALSE)
  f <- gsub(
    pattern = "    <ageGroup lowerbound=\"0\">\n      <group upperbound=\"5\"/>\n      <group upperbound=\"90\"/>\n    </ageGroup>",
    replacement = "    <ageGroup lowerbound=\"0\">\n      <group upperbound=\"5\"/>\n      <group upperbound=\"90\"/>\n    </ageGroup>\n    <cohorts>\n      <subPop id=\"LLINusers\" number=\"1\"/>\n    </cohorts>",
    x = f,
    fixed = TRUE
  )
  f <- gsub(
    pattern = "<deployment name=\"DDT test\">",
    replacement = "<component id=\"LLINusers\">\n        <recruitmentOnly/>\n      </component>\n      <deployment name=\"DDT test\">",
    x = f,
    fixed = TRUE
  )
  f <- gsub(
    pattern = "<component id=\"GVI\"/>",
    replacement = "<component id=\"GVI\"/>\n        <component id=\"LLINusers\"/>",
    x = f,
    fixed = TRUE
  )
  writeLines(f, con = file.path(rootDir, "exp_test_cohort_base.xml"))

  actual <- validateXML(
    xmlfile = file.path(rootDir, "exp_test_cohort_base.xml"),
    scenarios = scenarios
  )
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
