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


test_that("defineEntomology works", {
  testlist <- list(entomology = list())

  monthlyValues <- list(
    smoothing = "fourier"
  )
  for (i in c(5,4,3,2,1,2,3,5,8,13,11,7)) {
    monthlyValues <- append(monthlyValues, list(value = list(i)))
  }
  
  expected <- list(
    entomology = list(
      mode = "dynamic",
      name = "Namawala",
      scaledAnnualEIR = "23",
      vector = list(
        anopheles = list(
          mosquito = "gambiae_ss",
          propInfected = 0.078,
          propInfectious = 0.021,
          seasonality = list(
            annualEIR = 0.8,
            input = "EIR",
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
  actual <- defineEntomology(baseList = testlist, 
                             seasonalityParameters=list(
                               "gambiae_ss"=list(
                                 seasonality=c(5,4,3,2,1,2,3,5,8,13,11,7),
                                 annualEIR=0.8,
                                 propInfected=0.078,
                                 propInfectious=0.021)
                             ),
                             mosquitoParameters=list(
                               "gambiae_ss"=list(
                                 mosqRestDuration=list(value=3),
                                 extrinsicIncubationPeriod=list(value=11),
                                 mosqLaidEggsSameDayProportion=list(value=0.313),
                                 mosqSeekingDuration=list(value=0.33),
                                 mosqSurvivalFeedingCycleProbability=list(value=0.623),
                                 mosqProbBiting=list(mean=0.95,
                                                     variance=0),
                                 mosqProbFindRestSite=list(mean=0.95,
                                                           variance=0),
                                 mosqProbResting=list(mean=0.99,
                                                      variance=0),
                                 mosqProbOvipositing=list(mean=0.88),
                                 mosqHumanBloodIndex=list(mean=0.939))
                             ),
                             scaledAnnualEIR=23
                             )
  
  ## Normal
  expect_equal(actual, expected)
})
