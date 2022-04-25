### Little helpers for the the monitoring section

## These functions generate entries for the monitoring/continuous part which are
## tedious to define by hand.
## This includes:
##   - the options (for continuous and surveys), which can be passed as a list or
##     data frame
##   - the timepoints for monitoring
## The time points generation is more involved as it tries to take care that all
## generated dates are multiples of 5 days. This is done because openMalaria
## operates on 5 days intervals. openMalaria does accept dates as input but they
## are rounded internally to match the beforementioned 5 day steps (fine).
## However, openMalaria does not tell what the new date is and thus we lose this
## information (not fine). Thus, we generate here the dates and store them for
## later (postprocessing).

##' @title Generate list for 'monitoring/continuous/options'
##' @param period Value for period
##' @param options List of options
##' @export
monitoringContinuousGen <- function(period, options) {
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

##' @title Generate list for 'monitoring/continuous/options'
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
##' @param dateFilter Any of c("none", "weekly", "monthly", "quarterly",
##'   "yearly")
##' @keywords internal
##' @importFrom data.table ':=' .I
.xmlMonitoringTimeRegularSeq <- function(startDate, endDate, daysFilter,
                                         dateFilter = "none") {
  ## Appease NSE notes in R CMD check
  week <- month <- roll_date <- quarter_date <- year_date <- NULL
  
  ## Input verification
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertSubset(
    dateFilter,
    choices = c("none", "weekly", "monthly", "quarterly", "yearly"),
    add = assertCol
  )
  checkmate::reportAssertions(assertCol)
  
  ## Generate date sequence, 1 day resolution
  sequence <- seq.Date(as.Date(startDate), as.Date(endDate), by = "1 days")
  ## Remove and replace leap days
  sequence <- unique(gsub("(^\\d{4}\\-02)(-29)", "\\1-28", sequence))
  ## Add sequence to data frame
  dateDF <- data.table::data.table(date = as.Date(sequence))
  ## Add number of days since startDate
  dateDF$day <- seq(from = 0, length.out = nrow(dateDF))
  ## If weeks are requested, generate a column identifying the number of week a
  ## date is.
  if (dateFilter == "weekly") {
    weekNums <- rep(1:ceiling(nrow(dateDF) / 7), each = 7)
    dateDF <- dateDF[, week := weekNums[seq_len(nrow(dateDF))]]
  }
  ## We do the filtering twice because 1) we want only dates which are multiple
  ## of 5 and then 2) we might want to have 20 days steps. Could be more
  ## elegant.
  ## Keep only dates which are divisible by 5
  dateDF <- dateDF[dateDF$day %% 5 == 0 | dateDF$day == 0, ]
  ## Keep only dates which are divisible by daysFilter
  dateDF <- dateDF[dateDF$day %% daysFilter == 0 | dateDF$day == 0, ]
  ## Add number of time steps since startDate
  dateDF$timestep <- dateDF$day / 5
  
  ## Mofify output data according to arguments
  if (dateFilter == "weekly") {
    ## Only return one date per selected week, here the first one available by
    ## removing duplicates. The resulting differences in number of days should
    ## be a bit irregular, e.g. 5, 5, 5, 10 and so forth.
    ## Also remove the week column
    dateDF <- dateDF[!duplicated(dateDF$week), ][, week := NULL]
  } else if (dateFilter == "monthly") {
    ## We assume that the 15th of each month is the middle of the month. Then we
    ## calculate the difference of days for each date in relation to the 15th.
    ## Subsequently, we filter the dates to select the dates which have the
    ## minimum distance to the 15th of each month.
    dateDF <- dateDF[, month := abs(
      as.numeric(format(as.Date(dateDF$date), "%d")) - 15
    )]
    ## This is supposed to be faster than
    ## dates[, .SD[month == min(month)], by = data.table::month(dates$date)]
    ## Also sort the dates again and remove the month column
    dateDF <- dateDF[dateDF[, .I[month == min(month)],
                            by = data.table::month(dateDF$date)
    ]$V1][, month := NULL][order(date)]
    
  } else if (dateFilter == "quarterly") {
    ## Similar to above, generate a date sequence and only keep dates
    ## corresponding to the end of quarters.
    sequence <- seq.Date(as.Date(startDate), as.Date(endDate), by = "1 days")
    sequence <- unique(gsub("(^\\d{4}\\-02)(-29)", "\\1-28", sequence))
    quartersDF <- data.table::data.table(quarter_date = as.Date(sequence))
    ## Not pretty, but works
    quartersDF <- quartersDF[ifelse(format(as.Date(quartersDF$quarter_date), "%m-%d") == "03-31", TRUE,
                                    ifelse(format(as.Date(quartersDF$quarter_date), "%m-%d") == "06-30", TRUE,
                                           ifelse(format(as.Date(quartersDF$quarter_date), "%m-%d") == "09-30", TRUE,
                                                  ifelse(format(as.Date(quartersDF$quarter_date), "%m-%d") == "12-31", TRUE, FALSE)
                                           )
                                    )
    )]
    ## Add a column to do the rolling join on
    dateDF[, roll_date := date]
    quartersDF[, roll_date := quarter_date]
    ## Set key to join on
    data.table::setkey(dateDF, roll_date)
    data.table::setkey(quartersDF, roll_date)
    ## Perform join, roll to the nearest date before or equal to the end of the
    ## quarter.
    dateDF <- dateDF[quartersDF, roll = Inf]
    ## Make it pretty and set expected names
    dateDF <- dateDF[, c("roll_date", "quarter_date") := NULL]
    
  } else if (dateFilter == "yearly") {
    ## Similar to above, generate a date sequence and only keep dates
    ## corresponding to the end of a year.
    sequence <- seq.Date(as.Date(startDate), as.Date(endDate), by = "1 days")
    sequence <- unique(gsub("(^\\d{4}\\-02)(-29)", "\\1-28", sequence))
    yearDF <- data.table::data.table(year_date = as.Date(sequence))
    ## Not pretty, but works
    yearDF <- yearDF[ifelse(format(as.Date(yearDF$year_date), "%m-%d") == "12-31", TRUE, FALSE)]
    ## Add a column to do the rolling join on
    dateDF[, roll_date := date]
    yearDF[, roll_date := year_date]
    ## Set key to join on
    data.table::setkey(dateDF, roll_date)
    data.table::setkey(yearDF, roll_date)
    ## Perform join, roll to the nearest date before or equal to the end of the
    ## year.
    dateDF <- dateDF[yearDF, roll = Inf]
    ## Make it pretty and set expected names
    dateDF <- dateDF[, c("roll_date", "year_date") := NULL]
  }
  ## All modifications done, return data. Also, if dateFilter = "none", jump
  ## directly here.
  return(dateDF)
}

## TODO Add 'reported' option.
## TODO Correct documentation for interval

##' @title Generate list for 'monitoring/surveys/surveyTime'
##' @param startDate Start date as character "YYYY-MM-DD".
##' @param endDate End date as character "YYYY-MM-DD".
##' @param interval Time interval. Either a string ("daily", "weekly",
##'   "monthly", "quarterly", "yearly"), "X days" (e.g. "15 days"), or a list,
##'   e.g. (days = c(2, 5), months = c(3:7), years = c(2005:2030)). Setting the
##'   days in the list to 31 will always use the last day of the corresponding
##'   month (e.g. 28 for February, 31 for July), or a vector of survey dates
##' @param simStart Start date of the simulation. A good idea is to put this 100
##'   years in before your first survey date.
##' @param detectionLimit Deprecated in openMalaria. Double, limit above which a
##'   human's infection is reported as patent.
##' @param diagnostic Name of a parameterised diagnostic to use in surveys. See
##'   openMalaria documentation.
##' @param compatSurveys If survey times should be compatible to legacy
##'   versions. By default, surveys exclude the timestep of the survey date.
##'   Setting this to TRUE, one timestep will be added to each survey date so
##'   they include the dates of the survey itself.
##' @export
monitoringSurveyTimesGen <- function(startDate = NULL, endDate = NULL, 
                                     interval,simStart = NULL,
                                     detectionLimit = NULL, diagnostic = NULL, 
                                     compatSurveys = FALSE) {
  ## Input verification
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assert(
    checkmate::checkCharacter(interval),
    checkmate::checkList(interval),
    checkmate::checkDate(interval),
    add = assertCol
  )
  checkmate::reportAssertions(collection = assertCol)
  
  ## Helper function
  is.date<-function(x){!any(is.na(as.Date(x,format="%Y-%m-%d")))}
  
  ## Sanity check: simStart needs to be before startDate
  if (!is.null(simStart)) {
    if (is.null(startDate)) {
      if (is.list(interval)){
        checkDate <- paste(
          min(interval[["years"]]),
          min(interval[["months"]]),
          min(interval[["days"]]),
          sep = "-"
        )
      }
      if (is.date(interval)){
        checkDate <- min(interval)
      }
    } else {
      checkDate <- startDate
    }
    if (as.Date(checkDate) < as.Date(simStart)) {
      stop("startDate needs to be after simStart.")
    }
  }
  
  ## Flag if Open Malaria's repeatStep syntax should be used
  useRepeat <- FALSE
  ## Unit of the repeating step, e.g. "d" or "y"
  repeatUnit <- NULL
  ## Numeric stepsize, e.g. 1 or 20.027
  repeatStepsize <- NULL
  
  ## The generated sequence is either based on regular intervals and thus,
  ## specified via a string ("daily", "weekly", "monthly", "quarterly", "yearly"
  ## or "X days", e.g. "15 days") or as a list, which specifies the intervals.
  ## Here, daily = every 5 days, weekly = middle of a week, monthly = middle of
  ## a month, quarterly = close to end of quarter and yearly = close to end of
  ## year.
  
  ## REVIEW We increase the end year by so we can make sure that all deployments
  ##        were done and the effects have been measured.
  if (is.character(interval)&&!is.date(interval)) {
    endDate <- as.character(
      as.Date(paste(as.numeric(strsplit(endDate, split = "-")[[1]][1]) + 1,
                    as.numeric(strsplit(endDate, split = "-")[[1]][2]),
                    as.numeric(strsplit(endDate, split = "-")[[1]][3]),
                    sep = "-"
      ))
    )
    ## REVIEW I hate the following code from the bottom of my heart. If anyone
    ##        smarter than me comes up with a solution to the leap year issue
    ##        etc. feel free to put it here. Here's the kicker: We cannot simply
    ##        generate a date sequence using R's seq.Date because this would
    ##        correctly take leap years into account when we are calculating the
    ##        number of days between time points. Instead, we need to generate
    ##        our own 365 days long years and extract the dates we need.
    ##        Furthermore, if simStart is not NULL, use this as a starting date.
      if (interval %in% c("weekly", "monthly", "quarterly", "yearly")) {
        dates <- .xmlMonitoringTimeRegularSeq(
          startDate = ifelse(is.null(simStart), startDate, simStart),
          endDate = endDate,
          daysFilter = 5,
          dateFilter = interval
        )
        
        ## Remove dates which are before the requested startDate
        if (!is.null(simStart)) {
          dates <- dates[which.min(abs(as.Date(dates[, date]) - as.Date(startDate))):nrow(dates), ]
        }
        
        ## Get values for the xml entries
        days <- dates$day
        ## endDates is not used for these entries but mapply expects the same
        ## length of the inputs
        endDates <- rep(1, length.out = length(days))
      } else {
        
        ## 'daily' interval is an alias for '1 days'
        if (interval == "daily") {
          interval <- "1 days"
        }
        
        ## Check if given interval is a multiple of 5 days and adjust if necessary
        split <- strsplit(interval, split = " ")
        every <- as.numeric(split[[1]][1])
        unit <- split[[1]][2]
        if (unit == "day" | unit == "days") {
          useRepeat <- TRUE
          numDays <- every
          if (numDays %% 5 != 0) {
            ## Make sure numDays is >= 5
            numDays <- ifelse(numDays < 5, 5, round(numDays / 5) * 5)
            warning(paste0(
              "Interval must be a multiple of 5 and was adjusted to ",
              numDays,
              " days."
            ))
          }
          every <- numDays
          
          dates <- .xmlMonitoringTimeRegularSeq(
            startDate = ifelse(is.null(simStart), startDate, simStart),
            endDate = endDate,
            daysFilter = every
          )
          
          ## Remove dates which are before the requested startDate
          if (!is.null(simStart)) {
            dates <- dates[which.min(abs(as.Date(dates[, date]) - as.Date(startDate))):nrow(dates), ]
          }
          
          ## Get values for the xml entries
          days <- dates$day[1]
          endDates <- dates$date[length(dates$date)]
          repeatUnit <- "d"
          repeatStepsize <- every
        } else {
          stop("Unrecognized interval string")
        }
      }
    } else if (is.list(interval)) {
    ## Or the interval is a list of the form (days = c(2, 5), months = c(3:7),
    ## years = c(2005:2030)).
    
    ## Use repeat syntax
    useRepeat <- TRUE
    
    ## REVIEW As above, increase final year by one
    interval[["years"]] <- c(
      min(interval[["years"]]):(max(interval[["years"]]) + 1)
    )
    
    ## Furthermore, if simStart is not NULL, adjust start year
    if (!is.null(simStart)) {
      origStartDate <- paste(
        min(interval[["years"]]),
        min(interval[["months"]]),
        min(interval[["days"]]),
        sep = "-"
      )
      
      interval[["years"]] <- c(
        as.numeric(strsplit(simStart, split = "-")[[1]][1]):max(interval[["years"]])
      )
    }
    
    dates <- .xmlTimeBlockSeq(interval)
    
    ## As above, adjust dates so they are multiples of 5 day timesteps
    validDates <- .xmlMonitoringTimeRegularSeq(
      startDate = ifelse(is.null(simStart), dates[1], simStart),
      endDate = dates[length(dates)],
      daysFilter = 5
    )
    ## Adjust dates according to valid dates
    ## Adapted from https://stackoverflow.com/a/45082198
    dates <- unique(vapply(as.Date(dates), function(x) {
      return(as.character(validDates$date[which.min(abs(x - validDates$date))]))
    }, FUN.VALUE = character(1), USE.NAMES = FALSE))
    dates <- validDates[as.character(validDates$date) %in% dates, ]
    
    ## Remove dates which are before the requested startDate
    if (!is.null(simStart)) {
      dates <- dates[which.min(abs(as.Date(dates[, date]) - as.Date(origStartDate))):nrow(dates), ]
    }
    
    ## Only use dates from the first year.
    days <- subset(
      dates, format(as.Date(dates$date), "%Y") == min(
        format(as.Date(dates$date), "%Y")
      )
    )
    days <- days$day
    
    ## Extract the dates of the final year
    endDates <- subset(
      dates, format(as.Date(dates$date), "%Y") == max(
        format(as.Date(dates$date), "%Y")
      )
    )
    endDates <- endDates$date
    
    repeatUnit <- "y"
    repeatStepsize <- 1
  } else if (is.date(interval)){
    ## Or the interval is a vector of survey dates
    dates <- sort(interval)
    ## Use repeat syntax
    useRepeat <- F
    
    # ## REVIEW As above, increase final year by one
    # interval[["years"]] <- c(
    #   min(interval[["years"]]):(max(interval[["years"]]) + 1)
    # )
    # 
    ## Furthermore, if simStart is not NULL, adjust start year
    if (!is.null(simStart)) {
      origStartDate <- dates[1]
    }
    
    
    ## As above, adjust dates so they are multiples of 5 day timesteps
    validDates <- .xmlMonitoringTimeRegularSeq(
      startDate = if(is.null(simStart)){dates[1]}else{simStart},
      endDate = dates[length(dates)],
      daysFilter = 5
    )
    ## Adjust dates according to valid dates
    ## Adapted from https://stackoverflow.com/a/45082198
    dates <- unique(vapply(as.Date(dates), function(x) {
      return(as.character(validDates$date[which.min(abs(x - validDates$date))]))
    }, FUN.VALUE = character(1), USE.NAMES = FALSE))
    dates <- validDates[as.character(validDates$date) %in% dates, ]
    
    ## Remove dates which are before the requested startDate
    if (!is.null(simStart)) {
      dates <- dates[which.min(abs(as.Date(dates[, date]) - as.Date(origStartDate))):nrow(dates), ]
    }
    
    ## Days used for the xml file
    days <- dates$date
    ## Extract the dates of the final year
    endDates <- subset(
      dates, format(as.Date(dates$date), "%Y") == max(
        format(as.Date(dates$date), "%Y")
      )
    )
    endDates <- endDates$date
  }
  
  ## Store the dates in the cache
  assign(x = "surveyTimes", value = dates, envir = .pkgcache)
  
  if (useRepeat == TRUE) {
    ## Add 1 timestep = 5 days to the endDates which are used for repeatEnd. We do
    ## this to make sure that the very last survey is actually recorded.
    ## To quote the Open Malaria developer:
    ## The most important thing is that a survey will report events since the
    ## beginning of the time-step of the last survey.
    ## And from the schema definition:
    ## If present, the survey is repeated every repeatStep timesteps .... ending
    ## before repeatEnd (final repetition is the one before repeatEnd).
    ## https://swisstph.github.io/openmalaria/schema-43.html#end-of-repetition-exclusive
    endDates <- endDates + 5
  }
  
  if (compatSurveys == TRUE) {
    ## Adjust entries so they correspond to inclusive dates. By default,
    ## OpenMalaria handles survey dates exclusively; the survey ends before the
    ## given date.
    days <- days + 5
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
    if (!is.date(interval)){
    if (useRepeat == TRUE) {
      entry[["repeatStep"]] <- as.character(paste0(repeatStepsize, repeatUnit))
      entry[["repeatEnd"]] <- y
      entry <- append(
        entry,
        paste0(x, "d")
      )
    } else {
      entry <- append(
        entry,
        paste0(x, "d")
      )
    } 
    } else {
      entry <- append(
        entry,as.character(x)
      )
      }
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
  ## Input verification
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertSubset(
    interval,
    choices = c("month", "year", "quarter"),
    add = assertCol
  )
  checkmate::reportAssertions(assertCol)
  
  ## Start list
  startDate <- SIMSTART
  outlist <- list(
    name = name,
    startDate = startDate
  )
  
  ## Generate continuous content
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
      data = outlist, sublist = NULL, entry = "continuous",
      input = monitoringContinuousGen(period = 1, list(
        name = c(
          "input EIR", "simulated EIR", "human infectiousness", "immunity h",
          "immunity Y", "new infections", "num transmitting humans",
          "alpha", "P_B", "P_C*P_D"
        ),
        value = unlist(cout)
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
          value = c(
            cout$nHost, cout$nPatent, cout$nUncomp, cout$nSevere, cout$totalInfs,
            cout$totalPatentInf, cout$nNewInfections, cout$nTreatments1, cout$nTreatments2,
            cout$nTreatments3, cout$nTreatDeployments, cout$nHospitalSeqs,
            cout$nHospitalRecovs, cout$nHospitalDeaths, cout$nIndDeaths, cout$nDirDeaths,
            cout$expectedDirectDeaths, cout$expectedHospitalDeaths,
            cout$expectedIndirectDeaths, cout$expectedSevere, cout$simEIR,
            cout$inputEIR, cout$nMDAs, cout$nMassGVI, cout$nEPIVaccinations, cout$nMassIRS,
            cout$nMassITNs, cout$nMassVaccinations
          )
        )
      )
    )
  }
  
  outlist <- .xmlAddList(
    data = outlist, sublist = NULL, entry = "surveys",
    input = monitoringSurveyTimesGen(
      detectionLimit = detect, startDate = paste(y1, m1, d1, sep = "-"),
      endDate = paste(y2, m2, d2, sep = "-"), interval = paste0(interval, "ly"),
      simStart = SIMSTART, compatSurveys = TRUE
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