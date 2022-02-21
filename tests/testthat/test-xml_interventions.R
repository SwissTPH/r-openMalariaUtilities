test_that(".defineInterventionsHeader works", {
  testlist <- list(interventions = list())

  ## Normal
  expected <- list(interventions = list(name = "All interventions"))

  actual <- .defineInterventionsHeader(testlist)

  expect_equal(actual, expected)

  ## Overwriting
  testlist <- list(interventions = list(name = "foo"))
  expected <- list(interventions = list(name = "bar"))

  actual <- .defineInterventionsHeader(testlist, "bar")

  expect_equal(actual, expected)
})
