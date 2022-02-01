test_that(".placeholderseqGen works", {
  expected <- list(
    foo = c(
      "@foo2000@", "@foo2001@", "@foo2002@", "@foo2003@", "@foo2004@",
      "@foo2005@", "@foo2006@", "@foo2007@", "@foo2008@", "@foo2009@",
      "@foo2010@"
    )
  )

  actual <- .placeholderseqGen(
    x = list(
      foo = call("list", "foo", c(2000:2010))
    ),
    placeholders = "foo"
  )

  expect_equal(actual, expected)
})

test_that("deployIT works", {
  testlist <- list(interventions = list())

  expected <- list(
    interventions = list(
      human = list(
        deployment = list(
          name = "ITN",
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
      human = list(
        deployment = list(
          name = "ITN",
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
