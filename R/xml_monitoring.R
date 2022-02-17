### Little helpers for the the monitoring section

## These functions generate entries for the monitoring/continous part which are
## tedious to define by hand.
## This includes:
##   - the options (for continous and surveys), which can be passed as a list or
##     data frame
##   - the timepoints for monitoring
## The time points generation is more involved as it tries to take care that all
## generated dates are multiples of 5 days. This is done because openMalaria
## operates on 5 days intervals. openMalaria does accept dates as input but they
## are rounded internally to match the beforementioned 5 day steps (fine).
## However, openMalaria does not tell what the new date is and thus we lose this
## information (not fine). Thus, we generate here the dates and store them for
## later (postprocessing).

##' @title Generate list for 'monitoring/continous/options'
##' @param period Value for period
##' @param options List of options
##' @export
monitoringContinousGen <- function(period, options) {
  ## Input validation
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assert(
    checkmate::checkDataFrame(options),
    checkmate::checkList(options),
    add = assertCol
  )
  checkmate::reportAssertions(collection = assertCol)
  ## Assign period
  outlist <- list(period = period)
  ## Loop over row, generating an entry eacht time and appending it to outlist
  outlist <- .xmlAddChunks(
    outlist = outlist, element = "option",
    attributeList = options
  )
  return(outlist)
}

##' @title Generate list for 'monitoring/continous/options'
##' @param onlyNewEpisodes Value for onlyNewEpisodes
##' @param options List of options
##' @export
monitoringSurveyOptionsGen <- function(onlyNewEpisodes = NULL, options) {
  ## Input validation
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assert(
    checkmate::checkDataFrame(options),
    checkmate::checkList(options),
    add = assertCol
  )
  checkmate::reportAssertions(collection = assertCol)
  ## Assign onlyNewEpisodes
  if (!is.null(onlyNewEpisodes)) {
    outlist <- list(onlyNewEpisodes = onlyNewEpisodes)
  } else {
    outlist <- list()
  }
  ## Loop over row, generating an entry eacht time and appending it to outlist
  outlist <- .xmlAddChunks(
    outlist = outlist, element = "option",
    attributeList = options
  )
  return(outlist)
}

##' @title Generate a date sequence
##' @param startDate Start date in 'YYYY-MM-DD'
##' @param endDate End date in 'YYYY-MM-DD'
##' @param daysFilter Day differences to keep
##' @keywords internal
.xmlMonitoringTimeRegularSeq <- function(startDate, endDate, daysFilter) {
  ## Generate date sequence, 1 day resolution
  sequence <- seq.Date(as.Date(startDate), as.Date(endDate), by = "1 days")
  ## Remove and replace leap days
  sequence <- unique(gsub("(^\\d{4}\\-02)(-29)", "\\1-28", sequence))
  ## Add sequence to data frame
  dateDf <- data.frame(date = as.Date(sequence))
  ## Add number of days since startDate
  dateDf$daysDiff <- seq(from = 1, length.out = nrow(dateDf))
  ## Set first date's diff to 0 in order to keep it
  dateDf$daysDiff[1] <- 0
  ## Keep only dates which are divisible by 5
  dateDf <- dateDf[dateDf$daysDiff %% 5 == 0 | dateDf$daysDiff == 0, ]
  ## Keep only requested day differences
  dateDf <- dateDf[dateDf$daysDiff %% daysFilter == 0 | dateDf$daysDiff == 0, ]
  ## Add number of time steps since startDate
  dateDf$timestep <- dateDf$daysDiff / 5
  return(dateDf)
}

## TODO Add 'reported' option

##' @title Generate list for 'monitoring/surveys/surveyTime'
##' @param startDate Start date as character "YYYY-MM-DD".
##' @param endDate End date as character "YYYY-MM-DD".
##' @param interval Time interval. Either a string, e.g. "1 week", or a list,
##'   e.g. (days = c(2, 5), months = c(3:7), years = c(2005:2030)). Accepted
##'   string are day(s), week(s), month(s) and quarter(s). Setting the days in
##'   the list to 31 will always use the last day of the corresponding month
##'   (e.g. 28 for February, 31 for July).
##' @param detectionLimit Deprecated in openMalaria. Double, limit above which a
##'   human's infection is reported as patent.
##' @param diagnostic Name of a parameterised diagnostic to use in surveys. See
##'   openMalaria documentation.
##' @export
monitoringSurveyTimesGen <- function(startDate, endDate, interval,
                                     detectionLimit = NULL, diagnostic = NULL) {
  ## Input verification
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assert(
    checkmate::checkCharacter(interval),
    checkmate::checkList(interval),
    add = assertCol
  )
  checkmate::reportAssertions(collection = assertCol)
  ## The generated sequence is either based on regular intervals and thus,
  ## specified via a string (e.g. "2 weeks") or as a list, which specifies the
  ## intervals
  ## REVIEW We increase the end year by so we can make sure that all surveys
  ##        were done and have been measured
  useDays <- FALSE
  if (is.character(interval)) {
    endDate <- as.character(
      as.Date(paste(as.numeric(strsplit(endDate, split = "-")[[1]][1]) + 1,
        as.numeric(strsplit(endDate, split = "-")[[1]][2]),
        as.numeric(strsplit(endDate, split = "-")[[1]][3]),
        sep = "-"
      ))
    )
    ## REVIEW I hate the following code from the bottom of my heart. If anyone
    ##        smarter than me comes up with a solution to the leap year issue
    ##        etc. feel free to put it here.

    ## Check if given interval is a multiple of 5 days and adjust if necessary
    split <- strsplit(interval, split = " ")
    every <- as.numeric(split[[1]][1])
    unit <- split[[1]][2]
    if (unit == "day" | unit == "days") {
      useDays <- TRUE
      numDays <- every
      if (numDays %% 5 != 0) {
        numDays <- round(numDays / 5) * 5
        warning(paste0(
          "Interval must be a multiple of 5 and was adjusted to ",
          numDays,
          " days."
        ))
      }
      every <- numDays
    } else if (unit == "week" | unit == "weeks") {
      useDays <- TRUE
      numDays <- every * 7
      if (numDays %% 5 != 0) {
        numDays <- round(numDays / 5) * 5
        warning(paste0(
          "Interval must be a multiple of 5 and was adjusted to ",
          numDays,
          " days."
        ))
      }
      every <- numDays
    } else if (unit == "month" | unit == "months") {
      useDays <- TRUE
      numDays <- every * 30
      if (numDays %% 5 != 0) {
        numDays <- round(numDays / 5) * 5
        warning(paste0(
          "Interval must be a multiple of 5 and was adjusted to ",
          numDays,
          " days."
        ))
      }
      every <- numDays
    } else if (unit == "quarter" | unit == "quarters") {
      useDays <- TRUE
      numDays <- every * 90
      if (numDays %% 5 != 0) {
        numDays <- round(numDays / 5) * 5
        warning(paste0(
          "Interval must be a multiple of 5 and was adjusted to ",
          numDays,
          " days."
        ))
      }
      every <- numDays
    } else {
      stop("Unrecognized interval string")
    }

    ## Here's the kicker: We cannot simply generate a date sequence using R's
    ## seq.Date because this would correctly take leap years into account when
    ## we are calculating the number of days between time points. Instead, we
    ## need to generate our own 365 days long years and extract the dates we
    ## need.
    dates <- .xmlMonitoringTimeRegularSeq(
      startDate = startDate,
      endDate = endDate,
      daysFilter = every
    )

    ## Store the information in the cache
    assign(x = "surveyTimes", value = dates, envir = .pkgcache)

    ## Extract days
    days <- every
    endDates <- dates$date[length(dates$date)]
  } else {
    ## REVIEW As above, increase final year by one
    interval[["years"]] <- c(
      min(interval[["years"]]):(max(interval[["years"]]) + 1)
    )
    dates <- .xmlTimeBlockSeq(interval)
    ## As above, adjust dates so they are multiples of 5 day timesteps
    validDates <- .xmlMonitoringTimeRegularSeq(
      startDate = dates[1],
      endDate = dates[length(dates)],
      daysFilter = 5
    )
    ## Adjust dates according to valid dates
    ## Adapted from https://stackoverflow.com/a/45082198
    dates <- unique(vapply(as.Date(dates), function(x) {
      return(as.character(validDates$date[which.min(abs(x - validDates$date))]))
    }, FUN.VALUE = character(1), USE.NAMES = FALSE))
    dates <- validDates[as.character(validDates$date) %in% dates, ]

    ## Store the information in the cache
    assign(x = "surveyTimes", value = dates, envir = .pkgcache)

    ## Only use dates from the first year.
    days <- subset(
      dates, format(as.Date(dates$date), "%Y") == min(
        format(as.Date(dates$date), "%Y")
      )
    )
    days <- days$daysDiff
    ## Extract the dates of the final year
    endDates <- subset(
      dates, format(as.Date(dates$date), "%Y") == max(
        format(as.Date(dates$date), "%Y")
      )
    )
    endDates <- endDates$date
  }
  ## Construct output list
  outlist <- list()
  if (!is.null(detectionLimit)) {
    outlist[["detectionLimit"]] <- detectionLimit
  }
  if (!is.null(diagnostic)) {
    outlist[["diagnostic"]] <- diagnostic
  }
  outlist <- append(outlist, mapply(function(x, y) {
    entry <- list()
    entry[["repeatStep"]] <- if (useDays == TRUE) {
      as.character(paste0(days[[1]], "d"))
    } else {
      "1y"
    }
    entry[["repeatEnd"]] <- y
    entry <- append(
      entry,
      if (useDays == TRUE) {
        as.character(paste0(0, "d"))
      } else {
        paste0(x, "d")
      }
    )
    return(list(surveyTime = entry))
  }, x = days, y = endDates))

  ## Return output list
  return(outlist)
}


## Cohorts

##' @title Generate list for 'monitoring/Cohorts'
##' @param ids Vector containg cohort names
##' @export
monitoringCohortsGen <- function(ids) {
  ## Input validation
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(ids, add = assertCol)
  checkmate::assertVector(ids, max.len = 22, add = assertCol)
  checkmate::reportAssertions(collection = assertCol)
  maxNum <- length(ids) - 1
  subPops <- data.frame(
    id = ids,
    number = 2^(0:maxNum),
    stringsAsFactors = FALSE
  )
  outlist <- list()
  ## Loop over row, generating an entry eacht time and appending it to outlist
  outlist <- .xmlAddChunks(
    outlist = outlist, element = "subPop",
    attributeList = subPops
  )
  return(outlist)
}



## DEPRECATED
##' @title Writes the monitoring measures xml chunk
##' @description Wrapper function for write_continuous and write_SurveyOptions
##' @param baseList List with experiment data.
##' @param name Name of monitoring settings.
##' @param continuous Write measures for the continuous surveys
##' @param SurveyOptions Write measures for the SurveyOptions
##' @param surveyMeasures If NULL, default will be used
##' @param continuousMeasures If NULL, default will be used
##' @param y1 Year of the first date (surveys starting from year y1)
##' @param m1 Month of the first date
##' @param d1 Day of the first date
##' @param y2 Year of the end date (surveys continuing until year y2)
##' @param m2 Month of the end date
##' @param d2 Day of the end date
##' @param interval Interval "month" "year" "quarter"
##' @param SIMSTART Starting date of the simulations in the format "yyyy-mm-dd",
##'   default e.g. "1918-01-01"
##' @param detect Detection limit for parasites in the blood
##' @param upperbounds Defines the upper age bound per age group
##' @export
write_monitoring_compat <- function(baseList, name = "Annual Surveys",
                                    continuous = TRUE, SurveyOptions = TRUE,
                                    surveyMeasures = NULL,
                                    continuousMeasures = NULL, y1 = 2000,
                                    y2 = 2036, m1 = 1, m2 = 1, d1 = 1, d2 = 1,
                                    SIMSTART = "1918-01-01", interval = "month",
                                    detect = 200,
                                    upperbounds = c(1, 2, 5, 6, 10, 11, 100)) {
  ## Start list
  startDate <- SIMSTART
  outlist <- list(
    name = name,
    startDate = startDate
  )

  ## Generate continous content
  if (continuous == TRUE) {
    cout <- list(
      inputEIR = TRUE, simEIRcont = FALSE, humanInfect = FALSE, immunH = FALSE,
      immunY = FALSE, newInfect = FALSE, transmHuman = FALSE, alpha = FALSE,
      PB = FALSE, PCPD = FALSE
    )

    ## Defaults
    if (is.null(continuousMeasures)) {
      continuousMeasures <- c(
        "inputEIR", "simEIRcont", "humanInfect", "immunH", "immunY", "newInfect",
        "transmHuman"
      )
    }

    cout[names(cout) %in% continuousMeasures] <- TRUE
    cout <- lapply(cout, tolower)

    outlist <- .xmlAddList(
      data = outlist, sublist = NULL, entry = "continous",
      input = monitoringContinousGen(period = 1, list(
        name = c(
          "input EIR", "simulated EIR", "human infectiousness", "immunity h",
          "immunity Y", "new infections", "num transmitting humans",
          "alpha", "P_B", "P_C*P_D", "mean insecticide content"
        ),
        value = c(unlist(cout), NA)
      ))
    )
  }
  ## Generate survey options content
  if (SurveyOptions == TRUE) {
    cout <- list(
      nHost = TRUE,
      nPatent = TRUE,
      nUncomp = TRUE,
      nSevere = TRUE,
      totalInfs = TRUE,
      nNewInfections = TRUE,
      totalPatentInf = TRUE,
      nTreatments1 = TRUE,
      nTreatments2 = TRUE,
      nTreatments3 = TRUE,
      nTreatDeployments = TRUE,
      nHospitalDeaths = TRUE,
      nHospitalSeqs = TRUE,
      nHospitalRecovs = TRUE,
      nIndDeaths = TRUE,
      nDirDeaths = TRUE,
      expectedDirectDeaths = TRUE,
      expectedHospitalDeaths = TRUE,
      expectedIndirectDeaths = TRUE,
      expectedSevere = TRUE,
      nMassITNs = FALSE,
      nMassIRS = FALSE,
      nMDAs = FALSE,
      nMassGVI = FALSE,
      nMassVaccinations = FALSE,
      nEPIVaccinations = FALSE,
      inputEIR = FALSE,
      simEIR = FALSE
    )

    if (is.null(surveyMeasures)) {
      basics <- c("nHost", "nPatent", "nUncomp", "nSevere", "simEIR")
      allTreatments <- c("nTreatments1", "nTreatments2", "nTreatments3", "nTreatDeployments")
      allHospital <- c("nHospitalSeqs", "nHospitalRecovs", "nHospitalDeaths")
      allDeaths <- c("nHospitalDeaths", "nIndDeaths", "nDirDeaths")
      allExpDeaths <- c("expectedDirectDeaths", "expectedHospitalDeaths", "expectedIndirectDeaths")
      allExpDeaths <- c("expectedDirectDeaths", "expectedHospitalDeaths", "expectedIndirectDeaths")
      allInterventions <- c("nMassITNs", "nMassIRS", "nMDAs", "nMassGVI", "nMassVaccinations", "nEPIVaccinations")
      surveyMeasures <- c(basics, allTreatments, allHospital, allDeaths, allExpDeaths, "expectedSevere")
    }

    cout[names(cout) %in% surveyMeasures] <- TRUE
    cout <- lapply(cout, tolower)

    outlist <- .xmlAddList(
      data = outlist, sublist = NULL, entry = "SurveyOptions",
      input = monitoringSurveyOptionsGen(
        onlyNewEpisodes = NULL, options = list(
          name = c(
            "nHost", "nPatent", "nUncomp", "nSevere", "totalInfs",
            "totalPatentInf", "nNewInfections", "nTreatments1", "nTreatments2",
            "nTreatments3", "nTreatDeployments", "nHospitalSeqs",
            "nHospitalRecovs", "nHospitalDeaths", "nIndDeaths", "nDirDeaths",
            "expectedDirectDeaths", "expectedHospitalDeaths",
            "expectedIndirectDeaths", "expectedSevere", "simulatedEIR",
            "inputEIR", "nMDAs", "nMassGVI", "nEPIVaccinations", "nMassIRS",
            "nMassITNs", "nMassVaccinations"
          ),
          value = c(unlist(cout))
        )
      )
    )
  }

  outlist <- .xmlAddList(
    data = outlist, sublist = NULL, entry = "surveys",
    input = monitoringSurveyTimesGen(
      detectionLimit = detect, startDate = paste(y1, m1, d1, sep = "-"),
      endDate = paste(y2, m2, d2, sep = "-"), interval = paste0("1 ", interval)
    )
  )

  outlist <- .xmlAddList(
    data = outlist, sublist = NULL, entry = "ageGroup",
    input = ageGroupsGen(
      lowerbound = 0, ageGroups = data.frame(upperbound = upperbounds)
    )
  )
  
  baseList <- .xmlAddList(
    data = baseList, sublist = NULL, entry = "monitoring",
    input = outlist, append = FALSE
  )
  
  return(baseList)
}
