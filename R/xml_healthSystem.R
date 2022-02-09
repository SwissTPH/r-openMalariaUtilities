##' @title Writes the health system part
##' @param baseList List with experiment data.
##' @param name Name of health system
##' @param initACT Initial artemisinine combination therapy.
##' @param initQN Initial quinine.
##' @param initSelf Initial probability of self-treatment.
##' @param compACT Compliance artemisinine combination therapy.
##' @param compQN Compliance quinine.
##' @param compSelf Compliance to self-treatment.
##' @param pSeekOfficialCareUncomplicated1 Probability that a patient with newly
##'   incident uncomplicated disease seeks official care.
##' @param pSelfTreatUncomplicated Probability that a patient with uncomplicated
##'   disease without recent history of disease (i.e. first line) will
##'   self-treat.
##' @param pSeekOfficialCareUncomplicated2 Probability that a patient with
##'   recurrence of uncomplicated disease seeks official care.
##' @param pSeekOfficialCareSevere Probability that a patient with severe
##'   disease obtains appropriate care.
##' @export
defineHealthSystem <- function(baseList, name = "Tanzania ACT",
                               initACT = 1, initQN = 1, initSelf = 1,
                               compACT = 1, compQN = 1, compSelf = 1,
                               pSeekOfficialCareUncomplicated1,
                               pSelfTreatUncomplicated = 0.01821375,
                               pSeekOfficialCareUncomplicated2,
                               pSeekOfficialCareSevere = 0.48) {
  baseList <- .xmlAddList(
    data = baseList,
    sublist = "healthSystem",
    entry = "ImmediateOutcomes",
    input = list(
      name = name,
      drugRegimen = list(
        firstLine = "ACT",
        inpatient = "QN",
        secondLine = "ACT"
      ),
      initialACR = list(
        ACT = list(value = initACT),
        QN = list(value = initQN),
        selfTreatment = list(value = initSelf)
      ),
      compliance = list(
        ACT = list(value = compACT),
        QN = list(value = compQN),
        selfTreatment = list(value = compSelf)
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
        value = pSeekOfficialCareUncomplicated1
      ),
      pSelfTreatUncomplicated = list(
        value = pSelfTreatUncomplicated
      ),
      pSeekOfficialCareUncomplicated2 = list(
        value = pSeekOfficialCareUncomplicated2
      ),
      pSeekOfficialCareSevere = list(
        value = pSeekOfficialCareSevere
      )
    ),
    append = FALSE
  )

  baseList <- .xmlAddList(
    data = baseList,
    sublist = "healthSystem",
    entry = "CFR",
    input = list(
      ## REVIEW This is hardcoded for the time being. Should be accessible for
      ## the users
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
    append = FALSE
  )

  baseList <- .xmlAddList(
    data = baseList,
    sublist = "healthSystem",
    entry = "pSequelaeInpatient",
    input = list(
      ## REVIEW This is hardcoded for the time being. Should be accessible for
      ## the users
      group = list(lowerbound = 0.0, value = 0.0132),
      group = list(lowerbound = 5.0, value = 0.005)
    ),
    append = FALSE
  )

  return(baseList)
}

##' @rdname defineHealthSystem
##' @export
define_health_system <- defineHealthSystem

## DEPRECATED
##' @title Writes the health system part of the xml file
##' @param baseList List with experiment data.
##' @param init.a ACT initial value
##' @param init.q QN initial value
##' @param init.s self-treat initial value
##' @param comp.a Compliance value ACT
##' @param comp.q Compliance value QN
##' @param comp.s Compliance value self-treat
##' @param pSeekOfficialCareSevere Default value is 0.48
##' @param pSelfTreatUncomplicated Default value is 0.0182
##' @param access Defines the baseline effective treatment coverage
##' @export
write_healthsys_compat <- function(baseList, access = "@Access2000@",
                                   init.a = 1, init.q = 1, init.s = 1,
                                   comp.a = 1, comp.q = 1, comp.s = 1,
                                   pSeekOfficialCareSevere = 0.48,
                                   pSelfTreatUncomplicated = 0.01821375) {
  baseList <- defineHealthSystem(
    baseList = baseList, name = "Tanzania ACT",
    initACT = init.a, initQN = init.q, initSelf = init.s, compACT = comp.a,
    compQN = comp.q, compSelf = comp.s,
    pSeekOfficialCareUncomplicated1 = access,
    pSelfTreatUncomplicated = pSelfTreatUncomplicated,
    pSeekOfficialCareUncomplicated2 = access,
    pSeekOfficialCareSevere = pSeekOfficialCareSevere
  )

  return(baseList)
}
