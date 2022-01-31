test_that("defineIRS works", {
  testlist <- list(interventions = list())

  expected <- list(
    interventions = list(
      human = list(
        component = list(
          id = "Actellic50EC",
          name = "Actellic50EC",
          GVI = list(
            decay = 0.299,
            "function" = "weibull",
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
              deterrency = list(value = -0.28),
              preprandialKillingEffect = list(value = 0.23),
              postprandialKillingEffect = list(value = 0.38)
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
    decay = 0.299,
    "function" = "weibull",
    anophelesParams = list(
      mosquito = "gam_indoor",
      propActive = 0,
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
    component = "Actellic50EC", noeffect = "indoor"
  )

  expect_equal(actual, expected)

  ## Set 'noeffect' to NULL
  expected$interventions$human$component$GVI <- list(
    decay = 0.299,
    "function" = "weibull",
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
      human = list(
        component = list(
          id = "nothing",
          name = "nothing",
          GVI = list(
            decay = 1,
            "function" = "step",
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
            initialsecticide = list(
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
