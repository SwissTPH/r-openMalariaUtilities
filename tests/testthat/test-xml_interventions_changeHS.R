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
  expected <- list(
    interpolation = "linear",
    group = list("lowerbound" = 1, "value" = 2)
  )

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
  expected <- list(
    interpolation = "linear",
    group = list("lowerbound" = 1, "value" = 2)
  )

  expect_equal(actual, expected)
})


test_that("defineChangeHS works", {
  testlist <- list(interventions = list())

  expected <- list(
    interventions = list(
      changeHS = list(
        name = "Change in case management",
        timedDeployment = list(
          time = "2002-03-15",
          ImmediateOutcomes = list(
            name = "Tanzania ACT",
            drugRegimen = list(
              firstLine = "ACT",
              inpatient = "QN",
              secondLine = "ACT"
            ),
            initialACR = list(
              ACT = list(value = 1),
              QN = list(value = 1),
              selfTreatment = list(value = 1)
            ),
            compliance = list(
              ACT = list(value = 1),
              QN = list(value = 1),
              selfTreatment = list(value = 1)
            ),
            nonCompliersEffective = list(
              ACT = list(value = 0),
              selfTreatment = list(value = 0)
            ),
            treatmentActions = list(
              ACT = list(
                name = "clear blood-stage infections",
                clearfections = list(
                  stage = "blood",
                  timesteps = "1"
                )
              ),
              QN = list(
                name = "clear blood-stage infections",
                clearfections = list(
                  stage = "blood",
                  timesteps = "1"
                )
              )
            ),
            pSeekOfficialCareUncomplicated1 = list(
              value = 1
            ),
            pSelfTreatUncomplicated = list(
              value = 0.01821375
            ),
            pSeekOfficialCareUncomplicated2 = list(
              value = 1
            ),
            pSeekOfficialCareSevere = list(
              value = 0.48
            )
          ),
          CFR = list(
            group = list(lowerbound = 0, value = 0.09189),
            group = list(lowerbound = 0.25, value = 0.0810811),
            group = list(lowerbound = 0.75, value = 0.0648649),
            group = list(lowerbound = 1.5, value = 0.0689189),
            group = list(lowerbound = 2.5, value = 0.0675676),
            group = list(lowerbound = 3.5, value = 0.0297297),
            group = list(lowerbound = 4.5, value = 0.0459459),
            group = list(lowerbound = 7.5, value = 0.0945946),
            group = list(lowerbound = 12.5, value = 0.1243243),
            group = list(lowerbound = 15, value = 0.1378378)
          ),
          pSequelaeInpatient = list(
            group = list(lowerbound = 0.0, value = 0.0132),
            group = list(lowerbound = 5.0, value = 0.005)
          )
        )
      )
    )
  )

  actual <- defineChangeHS(
    baseList = testlist, interval = list(
      years = c(2002), months = 3, days = 15
    ), pSeekOfficialCareUncomplicated1 = 1, pSeekOfficialCareUncomplicated2 = 1
  )

  ## Normal
  expect_equal(actual, expected)
})


test_that("define_changeHS_compat works", {
  testlist <- list(interventions = list())

  expected <- list(
    interventions = list(
      changeHS = list(
        name = "Change in case management",
        timedDeployment = list(
          time = as.Date("2000-01-05"),
          ImmediateOutcomes = list(
            name = "Tanzania ACT",
            drugRegimen = list(
              firstLine = "ACT",
              inpatient = "QN",
              secondLine = "ACT"
            ),
            initialACR = list(
              ACT = list(value = 1),
              QN = list(value = 1),
              selfTreatment = list(value = 1)
            ),
            compliance = list(
              ACT = list(value = 1),
              QN = list(value = 1),
              selfTreatment = list(value = 1)
            ),
            nonCompliersEffective = list(
              ACT = list(value = 0),
              selfTreatment = list(value = 0)
            ),
            treatmentActions = list(
              ACT = list(
                name = "clear blood-stage infections",
                clearfections = list(
                  stage = "blood",
                  timesteps = "1"
                )
              ),
              QN = list(
                name = "clear blood-stage infections",
                clearfections = list(
                  stage = "blood",
                  timesteps = "1"
                )
              )
            ),
            pSeekOfficialCareUncomplicated1 = list(
              value = "@Access2000@"
            ),
            pSelfTreatUncomplicated = list(
              value = 0.01821375
            ),
            pSeekOfficialCareUncomplicated2 = list(
              value = "@Access2000@"
            ),
            pSeekOfficialCareSevere = list(
              value = 0.48
            )
          ),
          CFR = list(
            group = list(lowerbound = 0, value = 0.09189),
            group = list(lowerbound = 0.25, value = 0.0810811),
            group = list(lowerbound = 0.75, value = 0.0648649),
            group = list(lowerbound = 1.5, value = 0.0689189),
            group = list(lowerbound = 2.5, value = 0.0675676),
            group = list(lowerbound = 3.5, value = 0.0297297),
            group = list(lowerbound = 4.5, value = 0.0459459),
            group = list(lowerbound = 7.5, value = 0.0945946),
            group = list(lowerbound = 12.5, value = 0.1243243),
            group = list(lowerbound = 15, value = 0.1378378)
          ),
          pSequelaeInpatient = list(
            group = list(lowerbound = 0.0, value = 0.0132),
            group = list(lowerbound = 5.0, value = 0.005)
          )
        )
      )
    )
  )

  actual <- define_changeHS_compat(
    baseList = testlist,
    access = "Access", coverage = NULL,
    y1 = 2000, y2 = 2000, use_at_symbol = TRUE,
    pSelfTreatUncomplicated = 0.01821375,
    futSevere = NULL,
    pSeekOfficialCareSevere = 0.48,
    SIMSTART = "1918-01-01", every = 1,
    interval = "year", m1 = 1, m2 = 1,
    init.act = 1, init.qn = 1, init.self = 1,
    comp.act = 1, comp.qn = 1, comp.self = 1
  )

  ## Normal
  expect_equal(actual, expected)
})
