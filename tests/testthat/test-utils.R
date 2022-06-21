test_that("splitSeq works (no rest)", {
  actual <- splitSeq(1:20, 5)
  expected <- list(
    "1" = 1:5,
    "2" = 6:10,
    "3" = 11:15,
    "4" = 16:20
  )
  expect_equal(actual, expected)
})

test_that("splitSeq works (with rest)", {
  actual <- splitSeq(1:21, 5)
  expected <- list(
    "1" = 1:5,
    "2" = 6:10,
    "3" = 11:15,
    "4" = 16:20,
    "5" = 21
  )
  expect_equal(actual, expected)
})
