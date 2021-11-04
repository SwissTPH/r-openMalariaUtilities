test_that("changeHSCFRGen works (with interpolation)", {
  actual <- changeHSCFRGen(
    ageGroups = data.frame("lowerbound" = 1, "value" = 2)
  )
  expected <- list(group = list("lowerbound" = 1, "value" = 2))

  expect_equal(actual, expected)
})

test_that("changeHSCFRGen works (with interpolation)", {
  actual <- changeHSCFRGen(
    interpolation = "linear",
    ageGroups = data.frame("lowerbound" = 1, "value" = 2)
  )
  expected <- list(interpolation = "linear",
                   group = list("lowerbound" = 1, "value" = 2))

  expect_equal(actual, expected)
})


test_that("changeHSpSeqInGen works (with interpolation)", {
  actual <- changeHSpSeqInGen(
    ageGroups = data.frame("lowerbound" = 1, "value" = 2)
  )
  expected <- list(group = list("lowerbound" = 1, "value" = 2))

  expect_equal(actual, expected)
})

test_that("changeHSpSeqInGen works (with interpolation)", {
  actual <- changeHSpSeqInGen(
    interpolation = "linear",
    ageGroups = data.frame("lowerbound" = 1, "value" = 2)
  )
  expected <- list(interpolation = "linear",
                   group = list("lowerbound" = 1, "value" = 2))

  expect_equal(actual, expected)
})
