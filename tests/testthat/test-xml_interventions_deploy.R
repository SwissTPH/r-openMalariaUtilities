test_that(".placeholderseqGen works", {
  expected <- list(
    foo = c(
      "@foo2000@", "@foo2001@", "@foo2002@", "@foo2003@", "@foo2004@",
      "@foo2005@", "@foo2006@", "@foo2007@", "@foo2008@", "@foo2009@",
      "@foo2010@"
    )
  )

  actual <- .placeholderseqGen(
    x = list(foo = list("foo", c(2000:2010))),
    placeholders = "foo"
  )

  expect_equal(actual, expected)
})

test_that("deployIT works", {
  testlist <- list(interventions = list())

  expected <- list(
    interventions = list(
      name = "All interventions",
      human = list(
        deployment = list(
          name = "ITN",
          component = list(
            id = "ITN"
          ),
          timed = list(
            deploy = list(
              coverage = "@foo2001@",
              time = "2001-03-15"
            ),
            deploy = list(
              coverage = "@foo2002@",
              time = "2002-03-15"
            )
          )
        )
      )
    )
  )

  actual <- deployIT(
    baseList = testlist, interval = list(
      years = c(2001:2002), months = 3, days = 15
    ),
    coverage = list("foo", c(2001:2002))
  )

  ## Normal
  expect_equal(actual, expected)

  ## minAge and maxAge
  expected <- list(
    interventions = list(
      name = "All interventions",
      human = list(
        deployment = list(
          name = "ITN",
          component = list(
            id = "ITN"
          ),
          timed = list(
            deploy = list(
              coverage = "@foo2001@",
              time = "2001-03-15",
              minAge = 1,
              maxAge = 5
            ),
            deploy = list(
              coverage = "@foo2002@",
              time = "2002-03-15",
              minAge = 1,
              maxAge = 5
            )
          )
        )
      )
    )
  )

  actual <- deployIT(
    baseList = testlist, interval = list(
      years = c(2001:2002), months = 3, days = 15
    ),
    coverage = list("foo", c(2001:2002)), minAge = 1, maxAge = 5
  )

  expect_equal(actual, expected)
})

## DEPRECATED
test_that("deploy_it_compat works", {
  testlist <- list(interventions = list())

  ## Normal, dates generated
  expected <- list(
    interventions = list(
      name = "All interventions",
      human = list(
        deployment = list(
          name = "ITN",
          component = list(
            id = "ITN"
          ),
          timed = list(
            deploy = list(
              coverage = "futITNcov",
              time = as.Date("2021-06-05")
            ),
            deploy = list(
              coverage = "futITNcov",
              time = as.Date("2021-07-05")
            )
          )
        )
      )
    )
  )

  actual <- deploy_it_compat(
    testlist,
    component = "ITN",
    coverage = "futITNcov",
    byyear = FALSE,
    y1 = 2021, y2 = 2021, every = 1, interval = "month",
    m1 = 6, m2 = 7, d1 = 5, d2 = 5, SIMSTART = "1918-01-01"
  )

  expect_equal(actual, expected)

  ## deployvar used
  expected <- list(
    interventions = list(
      name = "All interventions",
      human = list(
        deployment = list(
          name = "ITN",
          component = list(
            id = "ITN"
          ),
          timed = list(
            deploy = list(
              coverage = "futITNcov",
              time = "@deploy_month2021@"
            )
          )
        )
      )
    )
  )

  actual <- deploy_it_compat(
    testlist,
    component = "ITN",
    coverage = "futITNcov",
    byyear = FALSE,
    deployvar = "deploy_month",
    y1 = 2021, y2 = 2021, every = 1, interval = "month",
    m1 = 6, m2 = 7, d1 = 5, d2 = 5, SIMSTART = "1918-01-01"
  )

  expect_equal(actual, expected)

  ## coverage placeholder
  expected <- list(
    interventions = list(
      name = "All interventions",
      human = list(
        deployment = list(
          name = "ITN",
          component = list(
            id = "ITN"
          ),
          timed = list(
            deploy = list(
              coverage = "@futITNcov2021@",
              time = "@deploy_month2021@"
            )
          )
        )
      )
    )
  )

  actual <- deploy_it_compat(
    testlist,
    component = "ITN",
    coverage = "@futITNcov@",
    byyear = TRUE,
    deployvar = "deploy_month",
    y1 = 2021, y2 = 2021, every = 1, interval = "month",
    m1 = 6, m2 = 7, d1 = 5, d2 = 5, SIMSTART = "1918-01-01"
  )

  expect_equal(actual, expected)

  ## cumulative
  expected <- list(
    interventions = list(
      name = "All interventions",
      human = list(
        deployment = list(
          name = "ITN",
          component = list(
            id = "ITN"
          ),
          timed = list(
            cumulativeCoverage = list(
              component = "ITN"
            ),
            deploy = list(
              coverage = "@futITNcov2021@",
              time = "@deploy_month2021@"
            )
          )
        )
      )
    )
  )

  actual <- deploy_it_compat(
    testlist,
    component = "ITN",
    coverage = "@futITNcov@",
    byyear = TRUE,
    deployvar = "deploy_month",
    y1 = 2021, y2 = 2021, every = 1, interval = "month",
    m1 = 6, m2 = 7, d1 = 5, d2 = 5, SIMSTART = "1918-01-01",
    cumulative = TRUE
  )

  expect_equal(actual, expected)

  ## cumulative + subpop
  expected <- list(
    interventions = list(
      name = "All interventions",
      human = list(
        deployment = list(
          name = "ITN",
          component = list(
            id = "ITN"
          ),
          timed = list(
            restrictToSubPop = list(
              id = "ITN-ITN"
            ),
            cumulativeCoverage = list(
              component = "ITN-ITN"
            ),
            deploy = list(
              coverage = "@futITNcov2021@",
              time = "@deploy_month2021@"
            )
          )
        )
      )
    )
  )

  actual <- deploy_it_compat(
    testlist,
    component = "ITN",
    coverage = "@futITNcov@",
    byyear = TRUE,
    deployvar = "deploy_month",
    y1 = 2021, y2 = 2021, every = 1, interval = "month",
    m1 = 6, m2 = 7, d1 = 5, d2 = 5, SIMSTART = "1918-01-01",
    cumulative = TRUE, subpop = TRUE
  )

  expect_equal(actual, expected)
})

test_that("deploy_cont_compat works", {
  testlist <- list(interventions = list())

  ## Normal, dates generated
  expected <- list(
    interventions = list(
      name = "All interventions",
      human = list(
        deployment = list(
          name = "IPTi",
          component = list(
            id = "IPTi"
          ),
          continuous = list(
            deploy = list(
              coverage = 0.8,
              targetAgeYrs = 0.8,
              begin = "2019-01-01",
              end = "2030-01-01"
            ),
            deploy = list(
              coverage = 0.7,
              targetAgeYrs = 0.16,
              begin = "2019-01-01",
              end = "2030-01-01"
            )
          )
        )
      )
    )
  )

  actual <- deploy_cont_compat(
    testlist,
    targetAgeYrs = c(0.8, 0.16),
    coverage = c(0.8, 0.7)
  )

  expect_equal(actual, expected)

  ## restrictToSubPop used
  expected <- list(
    interventions = list(
      name = "All interventions",
      human = list(
        deployment = list(
          name = "IPTi",
          component = list(
            id = "IPTi"
          ),
          continuous = list(
            restrictToSubPop = list(
              id = "foo1"
            ),
            deploy = list(
              coverage = 0.8,
              targetAgeYrs = 0.8,
              begin = "2019-01-01",
              end = "2030-01-01"
            ),
            deploy = list(
              coverage = 0.7,
              targetAgeYrs = 0.16,
              begin = "2019-01-01",
              end = "2030-01-01"
            )
          )
        )
      )
    )
  )

  actual <- deploy_cont_compat(
    testlist,
    targetAgeYrs = c(0.8, 0.16),
    coverage = c(0.8, 0.7),
    restrictToSubPop = "foo1"
  )

  expect_equal(actual, expected)
})
