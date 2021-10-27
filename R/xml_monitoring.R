### Create monitoring/continous entries

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
##' @param period TODO
##' @param options TODO
##' @return List for xml contruction
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
  outlist <- .xmlAddChunks(outlist = outlist, element = "option", attributeList = options)
  return(outlist)
}

##' @title Generate list for 'monitoring/continous/options'
##' @param onlyNewEpisodes TODO
##' @param options TODO
##' @return List for xml contruction
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
  outlist <- .xmlAddChunks(outlist = outlist, element = "option", attributeList = options)
  return(outlist)
}

## Adapted from https://stackoverflow.com/a/52067205
.leapYear <- function(year) {
  leap <- ifelse(test = (year %% 4 == 0 & year %% 100 != 0) | year %% 400 == 0,
    yes = TRUE,
    no = FALSE
  )
  return(leap)
}

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
    omuCache$surveyTimes <- dates

    ## Extract days
    days <- every
    endDates <- dates$date[length(dates$date)]
  } else {
    ## REVIEW As above, increase final year by one
    interval[["years"]] <- c(min(interval[["years"]]):(max(interval[["years"]]) + 1))
    dates <- .xmlTimeBlockSeq(interval)
    ## As above, adjust dates so they are multiples of 5 day timesteps
    validDates <- .xmlMonitoringTimeRegularSeq(
      startDate = dates[1],
      endDate = dates[length(dates)],
      daysFilter = 5
    )
    ## Adjust dates according to valid dates
    ## Adapted from https://stackoverflow.com/a/45082198
    dates <- unique(unlist(lapply(as.Date(dates), function(x) {
      as.character(validDates$date[which.min(abs(x - validDates$date))])
    })))
    dates <- validDates[as.character(validDates$date) %in% dates, ]

    ## Store the information in the cache
    omuCache$surveyTimes <- dates

    ## Only use dates from the first year.
    days <- subset(
      dates, format(as.Date(dates$date), "%Y") == min(format(as.Date(dates$date), "%Y"))
    )
    days <- days$daysDiff
    ## Extract the dates of the final year
    endDates <- subset(
      dates, format(as.Date(dates$date), "%Y") == max(format(as.Date(dates$date), "%Y"))
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
  mapply(function(x, y) {
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
    outlist <<- append(outlist, list(surveyTime = entry))
  }, x = days, y = endDates)
  ## Return output list
  return(outlist)
}

## Cohorts
## TODO Add documentation
##
##' @title TODO
##' @param ids TODO
##' @export
monitoringCohortsGen <- function(ids) {
  ## Input validation
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(ids, add = assertCol)
  checkmate::assertVector(ids, max.len = 22, add = assertCol)
  checkmate::reportAssertions(collection = assertCol)
  maxNum <- length(ids) - 1
  subPops <- data.frame(id = ids, number = 2^(0:maxNum))
  outlist <- list()
  ## Loop over row, generating an entry eacht time and appending it to outlist
  outlist <- .xmlAddChunks(outlist = outlist, element = "subPop", attributeList = subPops)
  return(outlist)
}
