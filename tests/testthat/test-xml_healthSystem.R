test_that("defineHealthSystem works", {
  testlist <- list(healthSystem = list())

  expected <- list(
    healthSystem = list(
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
            clearInfections = list(
              stage = "blood",
              timesteps = "1"
            )
          ),
          QN = list(
            name = "clear blood-stage infections",
            clearInfections = list(
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

  actual <- defineHealthSystem(
    baseList = testlist, pSeekOfficialCareUncomplicated1 = 1,
    pSeekOfficialCareUncomplicated2 = 1
  )

  ## Normal
  expect_equal(actual, expected)
})


test_that("write_healthsys_compat works", {
  testlist <- list(healthSystem = list())

  expected <- list(
    healthSystem = list(
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
            clearInfections = list(
              stage = "blood",
              timesteps = "1"
            )
          ),
          QN = list(
            name = "clear blood-stage infections",
            clearInfections = list(
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

  actual <- write_healthsys_compat(baseList = testlist)

  ## Normal
  expect_equal(actual, expected)
})
