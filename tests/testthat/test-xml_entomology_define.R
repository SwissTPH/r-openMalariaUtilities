test_that("make_ento_compat works", {
  testlist <- list(entomology = list())

  seasonality <- paste0("@m", 1:12, "@")
  monthlyValues <- list(
    smoothing = "fourier"
  )
  for (i in seasonality) {
    monthlyValues <- append(monthlyValues, list(value = list(i)))
  }

  expected <- list(
    entomology = list(
      mode = "dynamic",
      name = "Namawala",
      scaledAnnualEIR = "@EIR@",
      vector = list(
        anopheles = list(
          mosquito = "gambiaesl_indoor",
          propInfected = 0.078,
          propInfectious = 0.021,
          seasonality = list(
            input = "EIR",
            annualEIR = 0.81,
            monthlyValues = monthlyValues
          ),
          mosq = list(
            minInfectedThreshold = 0.001,
            mosqRestDuration = list(
              value = 3
            ),
            extrinsicIncubationPeriod = list(
              value = 11
            ),
            mosqLaidEggsSameDayProportion = list(
              value = 0.313
            ),
            mosqSeekingDuration = list(
              value = 0.33
            ),
            mosqSurvivalFeedingCycleProbability = list(
              value = 0.623
            ),
            availability = list(
              distr = "const"
            ),
            mosqProbBiting = list(
              mean = 0.95,
              variance = 0
            ),
            mosqProbFindRestSite = list(
              mean = 0.95,
              variance = 0
            ),
            mosqProbResting = list(
              mean = 0.99,
              variance = 0
            ),
            mosqProbOvipositing = list(
              value = 0.88
            ),
            mosqHumanBloodIndex = list(
              value = 0.939
            )
          ),
          nonHumanHosts = list(
            name = "unprotectedAnimals",
            mosqRelativeEntoAvailability = list(
              value = 1
            ),
            mosqProbBiting = list(
              value = 0.95
            ),
            mosqProbFindRestSite = list(
              value = 0.95
            ),
            mosqProbResting = list(
              value = 0.99
            )
          )
        ),
        nonHumanHosts = list(
          name = "unprotectedAnimals",
          number = 1.0
        )
      )
    )
  )
  actual <- make_ento_compat(baseList = testlist, mosqs = c("gambiaesl_indoor"))

  ## Normal
  expect_equal(actual, expected)
})
