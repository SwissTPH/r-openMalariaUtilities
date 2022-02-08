### Little helpers for the the interventions/changeHS section

##' @title Generate list for 'changeHS/CFR'
##' @param interpolation Value for interpolation
##' @param ageGroups Data frame
##' @export
changeHSCFRGen <- function(interpolation = NULL, ageGroups) {
  ## Input validation
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(ageGroups, add = assertCol)
  checkmate::reportAssertions(collection = assertCol)
  ## Assign interpolation
  if (!is.null(interpolation)) {
    outlist <- list(interpolation = interpolation)
  } else {
    outlist <- list()
  }
  ## Apply expected data types
  ageGroups[c("lowerbound", "value")] <- sapply(
    ageGroups[c("lowerbound", "value")], as.double
  )
  outlist <- .xmlAddChunks(
    outlist = outlist, element = "group",
    attributeList = ageGroups
  )
  return(outlist)
}

##' @title Generate list for 'changeHS/SpSeq'
##' @param interpolation Value for interpolation
##' @param ageGroups Data frame
##' @export
changeHSpSeqInGen <- function(interpolation = NULL, ageGroups) {
  ## Input validation
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(ageGroups, add = assertCol)
  checkmate::reportAssertions(collection = assertCol)
  ## Assign interpolation
  if (!is.null(interpolation)) {
    outlist <- list(interpolation = interpolation)
  } else {
    outlist <- list()
  }
  ## Apply expected data types
  ageGroups[c("lowerbound", "value")] <- sapply(
    ageGroups[c("lowerbound", "value")], as.double
  )
  outlist <- .xmlAddChunks(
    outlist = outlist, element = "group",
    attributeList = ageGroups
  )
  return(outlist)
}

##' @title Write the case management deployments
##' @param baseList List with experiment data.
##' @param name Name of the intervention
##' @param startDate Date in YYYY-MM-DD format.
##' @param endDate Date in YYYY-MM-DD format.
##' @param interval A string like '1 weeks'. Same as in [seq.Date()]. Or a list
##'   composed of the entries 'days' (optional), 'months' (optional) and
##'   'years'. If a list is used, startDate and endDate are not used and can be
##'   NULL.
##' @param dates If NULL, startDate, endDate and interval are used, else a
##'   vector of dates in YYYY-MM-DD format. Can be a placeholder.
##' @param initACT Initial artemisinine combination therapy. Can be a placeholder.
##' @param initQN Initial quinine. Can be a placeholder.
##' @param initSelf Initial probability of self-treatment. Can be a placeholder.
##' @param compACT Compliance artemisinine combination therapy. Can be a placeholder.
##' @param compQN Compliance quinine. Can be a placeholder.
##' @param compSelf Compliance to self-treatment. Can be a placeholder.
##' @param pSeekOfficialCareUncomplicated1 Probability that a patient with newly
##'   incident uncomplicated disease seeks official care. Can be a placeholder.
##' @param pSelfTreatUncomplicated Probability that a patient with uncomplicated
##'   disease without recent history of disease (i.e. first line) will
##'   self-treat. Can be a placeholder.
##' @param pSeekOfficialCareUncomplicated2 Probability that a patient with
##'   recurrence of uncomplicated disease seeks official care. Can be a placeholder.
##' @param pSeekOfficialCareSevere Probability that a patient with severe
##'   disease obtains appropriate care. Can be a placeholder.
##' @export
defineChangeHS <- function(baseList, name = "Change in case management",
                           startDate = NULL, endDate = NULL, interval,
                           dates = NULL, initACT = 1, initQN = 1, initSelf = 1,
                           compACT = 1, compQN = 1, compSelf = 1,
                           pSeekOfficialCareUncomplicated1,
                           pSelfTreatUncomplicated = 0.01821375,
                           pSeekOfficialCareUncomplicated2,
                           pSeekOfficialCareSevere = 0.48) {
  ## Generate a list containing the placeholder sequences from the function
  ## arguments.
  ## Get input arguments, remove function name from list and unwanted entries
  funArgs <- as.list(match.call()[-1])
  funArgs <- funArgs[!(names(funArgs) %in% c("baseList"))]
  ## Function arguments are unevaluated and can contain calls and symbols. Thus,
  ## we need to evaluate them before in the parent environment.
  for (arg in names(funArgs)) {
    funArgs[[arg]] <- eval(funArgs[[arg]], envir = parent.frame())
  }
  ## Generate list
  placeholderseq <- .placeholderseqGen(
    x = funArgs,
    placeholders = c(
      "dates", "initACT", "initQN", "initSelf", "compACT", "compQN", "compSelf",
      "pSeekOfficialCareUncomplicated1", "pSelfTreatUncomplicated",
      "pSeekOfficialCareUncomplicated2", "pSeekOfficialCareSevere"
    )
  )

  ## Generate date sequence, if NULL
  if (is.null(dates)) {
    dates <- xmlTimeGen(
      startDate = startDate,
      endDate = endDate,
      interval = interval
    )
  }
  ## Otherwise take them from the placeholders or assume that dates is a vector
  if (!is.null(placeholderseq[["dates"]])) {
    dates <- placeholderseq[["dates"]]
  }

  ## Check if the number of dates is equal or bigger than the longest
  ## placeholder sequence.
  placeholderseq <- .equalizePlaceholders(dates,
    placeholderseq = placeholderseq
  )

  ## Generate changeHS entry
  curChangeHS <- baseList[["interventions"]][["changeHS"]]
  ## If no current changeHS entry exisits, make a new one
  if (is.null(curChangeHS)) {
    curChangeHS <- list(
      name = name
    )
  }
  ## Generate output
  outlist <- list()
  outlist <- .xmlAddList(
    data = outlist, sublist = NULL,
    entry = NULL,
    input = list(
      name = name
    )
  )

  ## Generate an entry for each given date
  for (i in seq_len(length(dates))) {
    temp <- list(
      time = dates[[i]],
      ImmediateOutcomes = list(
        name = "Tanzania ACT",
        drugRegimen = list(
          firstLine = "ACT",
          inpatient = "QN",
          secondLine = "ACT"
        ),
        initialACR = list(
          ACT = list(value = if (!is.null(placeholderseq[["initACT"]])) {
            placeholderseq[["initACT"]][[i]]
          } else {
            initACT
          }),
          QN = list(value = if (!is.null(placeholderseq[["initQN"]])) {
            placeholderseq[["initQN"]][[i]]
          } else {
            initQN
          }),
          selfTreatment = list(value = if (!is.null(placeholderseq[["initSelf"]])) {
            placeholderseq[["initSelf"]][[i]]
          } else {
            initSelf
          })
        ),
        compliance = list(
          ACT = list(value = if (!is.null(placeholderseq[["compACT"]])) {
            placeholderseq[["compACT"]][[i]]
          } else {
            compACT
          }),
          QN = list(value = if (!is.null(placeholderseq[["compQN"]])) {
            placeholderseq[["compQN"]][[i]]
          } else {
            compQN
          }),
          selfTreatment = list(value = if (!is.null(placeholderseq[["compSelf"]])) {
            placeholderseq[["compSelf"]][[i]]
          } else {
            compSelf
          })
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
          value = if (!is.null(placeholderseq[["pSeekOfficialCareUncomplicated1"]])) {
            placeholderseq[["pSeekOfficialCareUncomplicated1"]][[i]]
          } else {
            pSeekOfficialCareUncomplicated1
          }
        ),
        pSelfTreatUncomplicated = list(
          value = if (!is.null(placeholderseq[["pSelfTreatUncomplicated"]])) {
            placeholderseq[["pSelfTreatUncomplicated"]][[i]]
          } else {
            pSelfTreatUncomplicated
          }
        ),
        pSeekOfficialCareUncomplicated2 = list(
          value = if (!is.null(
            placeholderseq[["pSeekOfficialCareUncomplicated2"]]
          )) {
            placeholderseq[["pSeekOfficialCareUncomplicated2"]][[i]]
          } else {
            pSeekOfficialCareUncomplicated2
          }
        ),
        pSeekOfficialCareSevere = list(
          value = if (!is.null(placeholderseq[["pSeekOfficialCareSevere"]])) {
            placeholderseq[["pSeekOfficialCareSevere"]][[i]]
          } else {
            pSeekOfficialCareSevere
          }
        )
      ),
      ## REVIEW This is hardcoded for the time being. Should be accessible for
      ## the users
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
      ## REVIEW This is hardcoded for the time being. Should be accessible for
      ## the users
      pSequelaeInpatient = list(
        group = list(lowerbound = 0.0, value = 0.0132),
        group = list(lowerbound = 5.0, value = 0.005)
      )
    )

    outlist <- .xmlAddList(
      data = outlist, sublist = c("timedDeployment"),
      entry = NULL,
      input = temp
    )
  }

  ## Add to base list
  baseList <- .xmlAddList(
    data = baseList, sublist = c("interventions"),
    entry = "changeHS", input = outlist
  )

  ## Return modified baseList
  return(baseList)
}

##' @rdname defineChangeHS
##' @export
define_changeHS <- defineChangeHS

## DEPRECATED
##' @title Function to write the case management deployments
##' @param baseList List with experiment data.
##' @param access Name of coverage value (i.e. "Access")
##' @param coverage Name of future intervention for change in HS (i.e.
##'   "@futCM@")
##' @param init.act Initial ACT
##' @param init.qn Initial Quinine
##' @param init.self Initial self treatment
##' @param comp.act See OpenMalaria
##' @param comp.qn See OpenMalaria
##' @param comp.self See OpenMalaria
##' @param use_at_symbol If TRUE, variable will be replaced with setting
##'   specific values
##' @param pSelfTreatUncomplicated Proportion of uncomplicated episodes treated
##'   at home
##' @param pSeekOfficialCareSevere Proportion of severe episodes that seek care
##'   at the formal sector
##' @param futSevere Variable for different values of 'pseekOfficialCareSevere'
##' @param y1 Year of the first date (surveys starting from year y1)
##' @param m1 Month of the first date
##' @param y2 Year of the end date (surveys continuing until year y2)
##' @param m2 Month of the end date
##' @param every Interval size
##' @param interval Interval size (days, weeks, )
##' @param SIMSTART Start of the simulations (equal to ORIGIN!)
##' @export
define_changeHS_compat <- function(baseList, access = "Access", coverage = NULL,
                                   y1 = 2000, y2 = 2015, use_at_symbol = TRUE,
                                   pSelfTreatUncomplicated = 0.01821375,
                                   futSevere = NULL,
                                   pSeekOfficialCareSevere = 0.48,
                                   SIMSTART = "1918-01-01", every = 1,
                                   interval = "year", m1 = 1, m2 = 1,
                                   init.act = 1, init.qn = 1, init.self = 1,
                                   comp.act = 1, comp.qn = 1, comp.self = 1) {

  ## Translate time information
  ## If no 'coverage' provided, then it won't write deployments for future year
  ## (i.e. year = y2 + 1)
  if (is.null(coverage)) {
    futyear <- 0
  } else {
    futyear <- 1
  }

  dates <- .deployTime_compat(
    y1 = y1, y2 = y2 + futyear, m1 = m1, m2 = m2, d1 = 5, d2 = 5, every = every,
    interval = interval
  )

  ## Year format (for specifying the name of the variable, i.e. futCM2021)
  years <- as.numeric(format(dates, "%Y"))
  yearid <- years

  ## If the occurence is more often than once a year, naming should include
  ## year, month, date
  if (interval != "year") {
    years <- gsub(dates, pattern = "\\-", replacement = "\\.")
  }
  ## Generate placeholders
  if (use_at_symbol == TRUE) {
    message(paste(
      "In your experiment, specify values for:",
      paste0(access, years, collapse = ", ")
    ))
  }

  if (use_at_symbol & !is.null(futSevere)) {
    message(paste(paste0(futSevere, years, collapse = ", ")))
  }

  if (!use_at_symbol & length(access) < length(y1:y2)) {
    stop("when 'use_at_symbol' = F,
        access needs to be a vector of numbers
        with an entry for each year in y1 to y2")
  }

  ## Initialize placeholders
  varname <- c()
  var2 <- c()
  init.a <- c()
  init.q <- c()
  init.s <- c()
  comp.a <- c()
  comp.q <- c()
  comp.s <- c()

  for (i in seq_len(length(dates))) {
    ## Assume that we want @ symbols around everything, e.g. @Access2002@
    if (use_at_symbol) {
      varname_tmp <- ifelse(
        yearid[i] <= y2,
        paste0("@", access, years[i], "@"),
        paste0("@", gsub(coverage, pattern = "@", replacement = ""), "@")
      )
    } else {
      varname_tmp <- ifelse(
        yearid[i] <= y2,
        access[i],
        paste0(gsub(coverage, pattern = "@", replacement = ""))
      )
    }
    ## Collect varname
    varname <- append(varname, varname_tmp)

    ## Other varying attributes
    var2_tmp <- ifelse(use_at_symbol & !is.null(futSevere), paste0("@", futSevere, years[i], "@"), pSeekOfficialCareSevere)
    init.a_tmp <- ifelse(use_at_symbol & !is.numeric(init.act), paste0("@", init.act, years[i], "@"), init.act)
    init.q_tmp <- ifelse(use_at_symbol & !is.numeric(init.qn), paste0("@", init.qn, years[i], "@"), init.qn)
    init.s_tmp <- ifelse(use_at_symbol & !is.numeric(init.self), paste0("@", init.self, years[i], "@"), init.self)
    comp.a_tmp <- ifelse(use_at_symbol & !is.numeric(comp.act), paste0("@", comp.act, years[i], "@"), comp.act)
    comp.q_tmp <- ifelse(use_at_symbol & !is.numeric(comp.qn), paste0("@", comp.qn, years[i], "@"), comp.qn)
    comp.s_tmp <- ifelse(use_at_symbol & !is.numeric(comp.self), paste0("@", comp.self, years[i], "@"), comp.self)

    ## Collect other placeholders
    var2 <- ifelse(is.numeric(var2_tmp), var2_tmp, append(var2, var2_tmp))
    init.a <- ifelse(is.numeric(init.a), init.a_tmp, append(init.a, init.a_tmp))
    init.q <- ifelse(is.numeric(init.q), init.q_tmp, append(init.q, init.q_tmp))
    init.s <- ifelse(is.numeric(init.s), init.s_tmp, append(init.s, init.s_tmp))
    comp.a <- ifelse(is.numeric(comp.a), comp.a_tmp, append(comp.a, comp.a_tmp))
    comp.q <- ifelse(is.numeric(comp.q), comp.q_tmp, append(comp.q, comp.q_tmp))
    comp.s <- ifelse(is.numeric(comp.s), comp.s_tmp, append(comp.s, comp.s_tmp))

    if (nchar(varname_tmp) < 3) {
      message(paste0("WARNING: varname = ", varname_tmp))
    }
  }

  baseList <- defineChangeHS(
    baseList = baseList, name = "Change in case management",
    dates = dates, initACT = init.a, initQN = init.q, initSelf = init.s,
    compACT = comp.a, compQN = comp.q, compSelf = comp.s,
    pSeekOfficialCareUncomplicated1 = varname,
    pSelfTreatUncomplicated = pSelfTreatUncomplicated,
    pSeekOfficialCareUncomplicated2 = varname,
    pSeekOfficialCareSevere = var2
  )

  return(baseList)
}
