actual <- monitoringContinousGen(1, list("bar" = "true", "baz" = "false"))
expected <- list(
  period = 1, option = list("bar" = "true", "baz" = "false")
)

test_that("monitoringContinousGen works", {
  expect_equal(actual, expected)
})

actual <- monitoringSurveyOptionsGen(
  options = list("bar" = "true", "baz" = "false")
)
expected <- list(
  option = list("bar" = "true", "baz" = "false")
)

test_that("monitoringSurveyOptionsGen works", {
  expect_equal(actual, expected)
})

actual <- monitoringCohortsGen(ids = c("foo", "bar", "baz"))
expected <- list(
  subPop = list(id = "foo", number = 1),
  subPop = list(id = "bar", number = 2),
  subPop = list(id = "baz", number = 4)
)

test_that("monitoringCohortsGen works", {
  expect_equal(actual, expected)
})

actual <- monitoringSurveyTimesGen("2001-01-01", "2002-01-01", "1 quarter")
expected <- list(surveyTime = list(
  repeatStep = "90d",
  repeatEnd = as.Date("2002-12-21"),
  "0d"
))

test_that("monitoringSurveyTimesGen works (regular interval)", {
  expect_equal(actual, expected)
})

actual <- monitoringSurveyTimesGen(interval = list(
  days = c(1, 10),
  months = c(3),
  years = c(2002)
))
expected <- list(
  surveyTime = list(
    repeatStep = "1y",
    repeatEnd = as.Date("2003-02-28"),
    "0d"
  ),
  surveyTime = list(
    repeatStep = "1y",
    repeatEnd = as.Date("2003-03-10"),
    "10d"
  )
)

test_that("monitoringSurveyTimesGen works (block interval)", {
  expect_equal(actual, expected)
})
