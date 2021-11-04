test_that("ageGroupsGen is working correctly", {
  test <- ageGroupsGen(0, data.frame("poppercent" = 1, "upperbound" = 2))
  expected <- list(
    lowerbound = 0, group = list("poppercent" = 1, "upperbound" = 2)
  )

  expect_equal(test, expected)
})
