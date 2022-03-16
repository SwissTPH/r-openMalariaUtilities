test_that("defineIRS works", {
  testlist <- list(interventions = list())

  expected <- list(
    interventions = list(
      name = "All interventions",
      human = list(
        component = list(
          id = "Actellic50EC",
          name = "Actellic50EC",
          GVI = list(
            decay = list(
              L = 0.299,
              "function" = "weibull"
            ),
            anophelesParams = list(
              mosquito = "gam_indoor",
              propActive = 1,
              deterrency = list(value = -0.28),
              preprandialKillingEffect = list(value = 0.23),
              postprandialKillingEffect = list(value = 0.38)
            ),
            anophelesParams = list(
              mosquito = "gam_outdoor",
              propActive = 0,
              deterrency = list(value = 0),
              preprandialKillingEffect = list(value = 0),
              postprandialKillingEffect = list(value = 0)
            )
          )
        )
      )
    )
  )

  actual <- defineIRS(
    baseList = testlist,
    mosquitos = c("gam_indoor", "gam_outdoor"),
    component = "Actellic50EC"
  )

  ## Normal
  expect_equal(actual, expected)

  ## Set 'noeffect' to indoor
  expected$interventions$human$component$GVI <- list(
    decay = list(
      L = 0.299,
      "function" = "weibull"
    ),
    anophelesParams = list(
      mosquito = "gam_indoor",
      propActive = 0,
      deterrency = list(value = 0),
      preprandialKillingEffect = list(value = 0),
      postprandialKillingEffect = list(value = 0)
    ),
    anophelesParams = list(
      mosquito = "gam_outdoor",
      propActive = 1,
      deterrency = list(value = -0.28),
      preprandialKillingEffect = list(value = 0.23),
      postprandialKillingEffect = list(value = 0.38)
    )
  )


  actual <- defineIRS(
    baseList = testlist,
    mosquitos = c("gam_indoor", "gam_outdoor"),
    component = "Actellic50EC", noeffect = "indoor"
  )

  expect_equal(actual, expected)

  ## Set 'noeffect' to NULL
  expected$interventions$human$component$GVI <- list(
    decay = list(
      L = 0.299,
      "function" = "weibull"
    ),
    anophelesParams = list(
      mosquito = "gam_indoor",
      propActive = 1,
      deterrency = list(value = -0.28),
      preprandialKillingEffect = list(value = 0.23),
      postprandialKillingEffect = list(value = 0.38)
    ),
    anophelesParams = list(
      mosquito = "gam_outdoor",
      propActive = 1,
      deterrency = list(value = -0.28),
      preprandialKillingEffect = list(value = 0.23),
      postprandialKillingEffect = list(value = 0.38)
    )
  )


  actual <- defineIRS(
    baseList = testlist,
    mosquitos = c("gam_indoor", "gam_outdoor"),
    component = "Actellic50EC", noeffect = NULL
  )

  expect_equal(actual, expected)
})

test_that("defineTreatSimple works", {
  testlist <- list(interventions = list())

  expected <- list(
    interventions = list(
      name = "All interventions",
      human = list(
        component = list(
          id = "MDA",
          name = "MDA",
          treatSimple = list(
            durationBlood = "15d",
            durationLiver = 0
          )
        )
      )
    )
  )

  actual <- defineTreatSimple(baseList = testlist)

  expect_equal(actual, expected)
})

test_that("defineNothing works", {
  testlist <- list(interventions = list())

  expected <- list(
    interventions = list(
      name = "All interventions",
      human = list(
        component = list(
          id = "nothing",
          name = "nothing",
          GVI = list(
            decay = list(
              L = 1,
              "function" = "step"
            ),
            anophelesParams = list(
              mosquito = "gam_indoor",
              propActive = 0,
              deterrency = list(value = 0),
              preprandialKillingEffect = list(value = 0),
              postprandialKillingEffect = list(value = 0)
            ),
            anophelesParams = list(
              mosquito = "gam_outdoor",
              propActive = 0,
              deterrency = list(value = 0),
              preprandialKillingEffect = list(value = 0),
              postprandialKillingEffect = list(value = 0)
            )
          )
        )
      )
    )
  )

  actual <- defineNothing(
    baseList = testlist,
    mosquitos = c("gam_indoor", "gam_outdoor")
  )

  expect_equal(actual, expected)
})

test_that("defineITN works", {
  testlist <- list(interventions = list())

  expected <- list(
    interventions = list(
      name = "All interventions",
      human = list(
        component = list(
          id = "histITN",
          name = "histITN",
          ITN = list(
            usage = list(value = 1),
            holeRate = list(
              CV = 0.9468,
              distr = "lognormal",
              mean = 1.8
            ),
            ripRate = list(
              CV = 0.9468,
              distr = "lognormal",
              mean = 1.8
            ),
            ripFactor = list(
              value = 0.3
            ),
            initialInsecticide = list(
              mean = 55.5,
              SD = 14,
              distr = "normal"
            ),
            insecticideDecay = list(
              "function" = "exponential",
              L = 3,
              CV = 0.9468
            ),
            attritionOfNets = list(
              "function" = "smooth-compact",
              L = 4,
              k = 2.143
            ),
            anophelesParams = list(
              mosquito = "gam_indoor",
              propActive = 1,
              twoStageDeterrency = list(
                entering = list(
                  insecticideFactor = 0.001,
                  insecticideScalingFactor = 0.003
                ),
                attacking = list(
                  insecticideFactor = -0.107,
                  insecticideScalingFactor = 0.2,
                  holeFactor = -0.406,
                  interactionFactor = 0.268,
                  holeScalingFactor = 0.018,
                  baseFactor = 0.876
                )
              ),
              preprandialKillingEffect = list(
                insecticideFactor = 0.413,
                insecticideScalingFactor = 0.097,
                holeFactor = 0.053,
                interactionFactor = 0.208,
                holeScalingFactor = 0.016,
                baseFactor = 0.036
              ),
              postprandialKillingEffect = list(
                insecticideFactor = 0.265,
                insecticideScalingFactor = 0.032,
                holeFactor = 0,
                interactionFactor = 0,
                holeScalingFactor = 0,
                baseFactor = 0.014
              )
            ),
            anophelesParams = list(
              mosquito = "gam_outdoor",
              propActive = 0,
              twoStageDeterrency = list(
                entering = list(
                  insecticideFactor = 0.001,
                  insecticideScalingFactor = 0.003
                ),
                attacking = list(
                  insecticideFactor = -0.107,
                  insecticideScalingFactor = 0.2,
                  holeFactor = -0.406,
                  interactionFactor = 0.268,
                  holeScalingFactor = 0.018,
                  baseFactor = 0.876
                )
              ),
              preprandialKillingEffect = list(
                insecticideFactor = 0.413,
                insecticideScalingFactor = 0.097,
                holeFactor = 0.053,
                interactionFactor = 0.208,
                holeScalingFactor = 0.016,
                baseFactor = 0.036
              ),
              postprandialKillingEffect = list(
                insecticideFactor = 0.265,
                insecticideScalingFactor = 0.032,
                holeFactor = 0,
                interactionFactor = 0,
                holeScalingFactor = 0,
                baseFactor = 0.014
              )
            )
          )
        )
      )
    )
  )

  actual <- defineITN(
    baseList = testlist,
    mosquitos = c("gam_indoor", "gam_outdoor")
  )

  expect_equal(actual, expected, tolerance = 0.001)
})

test_that("defineLarv works", {
  testlist <- list(interventions = list())

  expected <- list(
    interventions = list(
      name = "All interventions",
      vectorPop = list(
        intervention = list(
          name = "LSM",
          description = list(
            anopheles = list(
              mosquito = "gambiaesl_indoor",
              seekingDeathRateIncrease = list(
                initial = 0,
                decay = list(
                  L = 0.247,
                  "function" = "step"
                )
              ),
              probDeathOvipositing = list(
                initial = 0,
                decay = list(
                  L = 0.247,
                  "function" = "step"
                )
              ),
              emergenceReduction = list(
                initial = "@futLSMcov@",
                decay = list(
                  "function" = "step",
                  L = 0.25
                )
              )
            )
          ),
          timed = list(
            deploy = list(
              time = "2020-06-05"
            )
          )
        )
      )
    )
  )

  actual <- define_larv(
    testlist,
    mosquitos = c("gambiaesl_indoor"), component = "LSM",
    coverage = "@futLSMcov@", decayVals = list(
      L = .25, k = NULL, funct = "step"
    ), interval = list(
      years = 2020, months = 6, days = 5
    )
  )

  expect_equal(actual, expected, tolerance = 0.05)
})

## DEPRECATED
test_that("define_larv_compat works", {
  testlist <- list(interventions = list())

  expected <- list(
    interventions = list(
      name = "All interventions",
      vectorPop = list(
        intervention = list(
          name = "LSM",
          description = list(
            anopheles = list(
              mosquito = "gambiaesl_indoor",
              seekingDeathRateIncrease = list(
                initial = 0,
                decay = list(
                  L = 0.247,
                  "function" = "step"
                )
              ),
              probDeathOvipositing = list(
                initial = 0,
                decay = list(
                  L = 0.247,
                  "function" = "step"
                )
              ),
              emergenceReduction = list(
                initial = "@futLSMcov@",
                decay = list(
                  "function" = "step",
                  L = 0.25
                )
              )
            )
          ),
          timed = list(
            deploy = list(
              time = as.Date("2020-06-05")
            )
          )
        )
      )
    )
  )

  actual <- define_larv_compat(testlist,
    mosqs = c("gambiaesl_indoor"),
    component = "LSM",
    coverage = "@futLSMcov@",
    decayVals = list(L = 0.25, k = NULL, funct = "step"),
    y1 = 2020, y2 = 2020, m1 = 6, m2 = 6,
    every = 1, interval = "year"
  )

  expect_equal(actual, expected, tolerance = 0.05)
})

test_that("defineImportedInfections works", {
  testlist <- list(interventions = list())

  expected <- list(
    interventions = list(
      name = "All interventions",
      importedInfections = list(
        name = "importedInfections",
        timed = list(
          rate = list(
            value = 10,
            time = 0
          )
        )
      )
    )
  )

  actual <- defineImportedInfections(testlist)

  expect_equal(actual, expected)
})

## DEPRECATED
test_that("define_importedInfections_compat works", {
  testlist <- list(interventions = list())

  expected <- list(
    interventions = list(
      name = "All interventions",
      importedInfections = list(
        name = "importedInfections",
        timed = list(
          rate = list(
            value = 10,
            time = 0
          )
        )
      )
    )
  )

  actual <- define_importedInfections_compat(testlist)

  expect_equal(actual, expected)
})
