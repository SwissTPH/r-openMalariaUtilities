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
    experiment = testlist,
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
    experiment = testlist,
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
    experiment = testlist,
    mosquitos = c("gam_indoor", "gam_outdoor"),
    component = "Actellic50EC", noeffect = NULL
  )

  expect_equal(actual, expected)
})