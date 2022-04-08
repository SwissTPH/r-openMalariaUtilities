actual <- monitoringContinuousGen(1, list("bar" = "true", "baz" = "false"))
expected <- list(
  period = 1, option = list("bar" = "true", "baz" = "false")
)

test_that("monitoringContinuousGen works", {
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

actual <- monitoringSurveyTimesGen("2001-01-01", "2001-12-31", "monthly")
expected <- list(
  surveyTime = list("20d"),
  surveyTime = list("50d"),
  surveyTime = list("80d"),
  surveyTime = list("110d"),
  surveyTime = list("140d"),
  surveyTime = list("170d"),
  surveyTime = list("200d"),
  surveyTime = list("230d"),
  surveyTime = list("260d"),
  surveyTime = list("290d"),
  surveyTime = list("325d"),
  surveyTime = list("355d"),
  surveyTime = list("385d"),
  surveyTime = list("415d"),
  surveyTime = list("445d"),
  surveyTime = list("475d"),
  surveyTime = list("505d"),
  surveyTime = list("535d"),
  surveyTime = list("565d"),
  surveyTime = list("595d"),
  surveyTime = list("625d"),
  surveyTime = list("655d"),
  surveyTime = list("690d"),
  surveyTime = list("720d")
)

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

test_that("write_monitoring_compat works", {
  testlist <- list(monitoring = list())

  expected <- list(
    monitoring = list(
      name = "Annual Surveys",
      startDate = "1918-01-01",
      continuous = list(
        period = 1,
        option = list(name = "input EIR", value = "true"),
        option = list(name = "simulated EIR", value = "true"),
        option = list(name = "human infectiousness", value = "true"),
        option = list(name = "immunity h", value = "true"),
        option = list(name = "immunity Y", value = "true"),
        option = list(name = "new infections", value = "true"),
        option = list(name = "num transmitting humans", value = "true"),
        option = list(name = "alpha", value = "false"),
        option = list(name = "P_B", value = "false"),
        option = list(name = "P_C*P_D", value = "false")
      ),
      SurveyOptions = list(
        option = list(name = "nHost", value = "true"),
        option = list(name = "nPatent", value = "true"),
        option = list(name = "nUncomp", value = "true"),
        option = list(name = "nSevere", value = "true"),
        option = list(name = "totalInfs", value = "true"),
        option = list(name = "totalPatentInf", value = "true"),
        option = list(name = "nNewInfections", value = "true"),
        option = list(name = "nTreatments1", value = "true"),
        option = list(name = "nTreatments2", value = "true"),
        option = list(name = "nTreatments3", value = "true"),
        option = list(name = "nTreatDeployments", value = "true"),
        option = list(name = "nHospitalSeqs", value = "true"),
        option = list(name = "nHospitalRecovs", value = "true"),
        option = list(name = "nHospitalDeaths", value = "true"),
        option = list(name = "nIndDeaths", value = "true"),
        option = list(name = "nDirDeaths", value = "true"),
        option = list(name = "expectedDirectDeaths", value = "true"),
        option = list(name = "expectedHospitalDeaths", value = "true"),
        option = list(name = "expectedIndirectDeaths", value = "true"),
        option = list(name = "expectedSevere", value = "true"),
        option = list(name = "simulatedEIR", value = "true"),
        option = list(name = "inputEIR", value = "false"),
        option = list(name = "nMDAs", value = "false"),
        option = list(name = "nMassGVI", value = "false"),
        option = list(name = "nEPIVaccinations", value = "false"),
        option = list(name = "nMassIRS", value = "false"),
        option = list(name = "nMassITNs", value = "false"),
        option = list(name = "nMassVaccinations", value = "false")
      ),
      surveys = list(
        detectionLimit = 200,
        surveyTime = list("29920d"),
        surveyTime = list("29950d"),
        surveyTime = list("29980d"),
        surveyTime = list("30010d"),
        surveyTime = list("30040d"),
        surveyTime = list("30070d"),
        surveyTime = list("30100d"),
        surveyTime = list("30130d"),
        surveyTime = list("30160d"),
        surveyTime = list("30190d"),
        surveyTime = list("30220d"),
        surveyTime = list("30255d"),
        surveyTime = list("30285d"),
        surveyTime = list("30315d"),
        surveyTime = list("30345d"),
        surveyTime = list("30375d"),
        surveyTime = list("30405d"),
        surveyTime = list("30435d"),
        surveyTime = list("30465d"),
        surveyTime = list("30495d"),
        surveyTime = list("30525d"),
        surveyTime = list("30555d"),
        surveyTime = list("30585d"),
        surveyTime = list("30620d"),
        surveyTime = list("30650d")
      ),
      ageGroup = list(
        lowerbound = 0,
        group = list(upperbound = 1),
        group = list(upperbound = 2),
        group = list(upperbound = 5),
        group = list(upperbound = 6),
        group = list(upperbound = 10),
        group = list(upperbound = 11),
        group = list(upperbound = 100)
      )
    )
  )

  actual <- write_monitoring_compat(baseList = testlist, y2 = 2001)

  expect_equal(actual, expected)
})
