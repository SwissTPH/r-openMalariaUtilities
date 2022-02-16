test_that("ageGroupsGen works", {
  actual <- ageGroupsGen(0, data.frame("poppercent" = 1, "upperbound" = 2))
  expected <- list(
    lowerbound = 0, group = list("poppercent" = 1, "upperbound" = 2)
  )

  expect_equal(actual, expected)
})

test_that("defineDemography works", {
  testlist <- list(demography = list())

  ## With growthRate
  expected <- list(
    demography = list(
      name = "foo",
      popSize = 3000L,
      maximumAgeYrs = 90,
      growthRate = 0.5,
      ageGroup = list(
        lowerbound = 1,
        group = list("poppercent" = 1, "upperbound" = 2)
      )
    )
  )

  actual <- defineDemography(
    baseList = testlist, name = "foo", popSize = 3000L, maximumAgeYrs = 90,
    growthRate = 0.5, lowerbound = 1, poppercent = 1, upperbound = 2
  )

  expect_equal(actual, expected)

  ## With placeholders
  expected <- list(
    demography = list(
      name = "foo",
      popSize = "@pop@",
      maximumAgeYrs = "@maxYears@",
      growthRate = "@growthRate@",
      ageGroup = list(
        lowerbound = "@lowerbound@",
        group = list("poppercent" = 1, "upperbound" = 2)
      )
    )
  )

  actual <- defineDemography(
    baseList = testlist, name = "foo", popSize = "@pop@",
    maximumAgeYrs = "@maxYears@", growthRate = "@growthRate@",
    lowerbound = "@lowerbound@", poppercent = 1, upperbound = 2
  )

  expect_equal(actual, expected)
})
