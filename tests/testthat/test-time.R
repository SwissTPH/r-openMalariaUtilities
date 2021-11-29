actual <- .xmlTimeRegularSeq("2001-01-01", "2002-01-01", "1 quarter")
expected <- as.character(seq.Date(as.Date("2001-01-01"), as.Date("2002-01-01"), "1 quarter"))

test_that(".xmlTimeRegularSeq works", {
  expect_equal(actual, expected)
})

actual <- .xmlTimeBlockSeq(interval = list(days = c(1, 10),
                                           months = c(4:5),
                                           years = c(2002:2003)))
expected <- c("2002-04-01", "2002-04-10", "2002-05-01", "2002-05-10",
              "2003-04-01", "2003-04-10", "2003-05-01", "2003-05-10")

test_that(".xmlTimeBlockSeq works", {
  expect_equal(actual, expected)
})

test_that("xmlTimeGen works (regular seq)", {
  actual <- xmlTimeGen("2001-01-01", "2002-01-01", "1 quarter")
  expected <- as.character(seq.Date(as.Date("2001-01-01"), as.Date("2002-01-01"), "1 quarter"))

  expect_equal(actual, expected)
})

test_that("xmlTimeGen works (block seq)", {
  actual <- xmlTimeGen(interval = list(days = c(1, 10),
                                       months = c(4:5),
                                       years = c(2002:2003)))
  expected <- c("2002-04-01", "2002-04-10", "2002-05-01", "2002-05-10",
                "2003-04-01", "2003-04-10", "2003-05-01", "2003-05-10")

  expect_equal(actual, expected)
})
