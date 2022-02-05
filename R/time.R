### Time series generation

## These functions generate date sequences which should be compatible with
## openMalaria's handling of time.

## Setting this to 1st January of some year might simplify usage of dates, and
## putting the start a couple of years before the start of intervention
## deployment (along with some extra surveys) may be useful to check
## transmission stabilises to the expected pre-intervention levels

##' @title Generate date sequence based on start and end date
##' @param startDate Date in YYYY-MM-DD format
##' @param endDate Date in YYYY-MM-DD format
##' @param interval A string like '1 weeks'. Same as in [seq.Date()]
##' @return A sequence of dates in the YYYY-MM-DD format
##' @keywords internal
.xmlTimeRegularSeq <- function(startDate, endDate, interval) {
  ## Input verification
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assert(
    checkmate::checkCharacter(
      startDate,
      pattern = "^\\d{4}\\-\\d{2}\\-\\d{2}"
    ),
    checkmate::checkDate(
      startDate
    ),
    add = assertCol
  )
  checkmate::assert(
    checkmate::checkCharacter(
      endDate,
      pattern = "^\\d{4}\\-\\d{2}\\-\\d{2}"
    ),
    checkmate::checkDate(
      endDate
    ),
    add = assertCol
  )
  checkmate::assertCharacter(interval, add = assertCol)
  checkmate::reportAssertions(collection = assertCol)
  ## Convert to date class
  startDate <- as.Date(startDate)
  endDate <- as.Date(endDate)
  ## Date sanity check
  if (startDate > endDate) {
    stop("'startDate' cannot be larger than 'endDate'")
  }
  ## Check if given interval is below 5 days
  split <- strsplit(interval, split = " ")
  every <- as.numeric(split[[1]][1])
  unit <- split[[1]][2]
  if (every < 5 & (unit == "day" | unit == "days")) {
    warning("Defined interval is below the smallest possible interval and was adjusted to 5 days.")
    interval <- "5 days"
  }
  ## Generate dates
  sequence <- seq.Date(
    as.Date(startDate),
    as.Date(endDate), interval
  )
  ## Remove YYYY-02-29 if present and replace with YYYY-02-28, then remove
  ## duplicate dates
  sequence <- unique(gsub("(^\\d{4}\\-02)(-29)", "\\1-28", sequence))
  return(as.character(sequence))
}

##' @title Generate date sequence based on specified days, months and years
##' @param interval A list composed of the entries 'days' (optional), 'months'
##'   (optional) and 'years'
##' @return A sequence of dates in the YYYY-MM-DD format
.xmlTimeBlockSeq <- function(interval) {
  ## Input verification
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertSubset(
    names(interval),
    choices = c("days", "months", "years"),
    add = assertCol
  )
  checkmate::reportAssertions(collection = assertCol)
  ## Assume first of month and all months in a year if not specified
  if (is.null(interval[["days"]])) {
    interval[["days"]] <- 1
  }
  if (is.null(interval[["months"]])) {
    interval[["months"]] <- 1:12
  }
  ## Build date sequence
  dateSequence <- unlist(
    lapply(interval[["years"]], function(y) {
      sapply(interval[["months"]], function(m) {
        sapply(interval[["days"]], function(d) {
          paste(y, m, d, sep = "-")
        })
      })
    })
  )
  ## Make sure that the maximum number of days in a given month is respected
  thirties <- c(4, 6, 9, 11)
  thirtyones <- c(1, 3, 5, 7, 8, 10, 12)
  sapply(dateSequence, function(date) {
    year <- as.numeric(strsplit(date, split = "-")[[1]][1])
    month <- as.numeric(strsplit(date, split = "-")[[1]][2])
    day <- as.numeric(strsplit(date, split = "-")[[1]][3])
    if (month %in% thirties & day == 31) {
      dateSequence[dateSequence == date] <- paste(year, month, 30, sep = "-")
    } else if (month %in% thirtyones & day == 30) {
      dateSequence[dateSequence == date] <- paste(year, month, 31, sep = "-")
      ## February is a special case
    } else if (month == 2 & day %in% c(29, 30, 31)) {
      dateSequence[dateSequence == date] <- paste(year, month, 28, sep = "-")
    }
  })
  ## Convert to dates
  dateSequence <- as.Date(dateSequence)
  ## Remove YYYY-02-29 if present and replace with YYYY-02-28, then remove
  ## duplicate dates
  dateSequence <- unique(gsub("(^\\d{4}\\-02)(-29)", "\\1-28", dateSequence))
  ## Remove possible NAs, due to invalid dates
  dateSequence <- stats::na.omit(dateSequence)
  return(dateSequence)
}

## Time series until the last valid date (e.g. endDate = 2002-03-29 ->
## 2002-03-25)
##' @title Generate date sequence based on start and end date
##' @param startDate Date in YYYY-MM-DD format
##' @param endDate Date in YYYY-MM-DD format
##' @param interval A string like '1 weeks'. Same as in [seq.Date()]
##' @return A sequence of dates in the YYYY-MM-DD format
##' @export
xmlTimeGen <- function(startDate = NULL, endDate = NULL, interval) {
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
  if (is.character(interval)) {
    out <- .xmlTimeRegularSeq(startDate, endDate, interval)
  } else {
    out <- .xmlTimeBlockSeq(interval)
  }
  return(out)
}


#
## Compat functions

##' @title Basic function used for deploying interventions
##' @description Compatibility with Munirflow deploy time format. Returns a list
##'   with a startDate, endDate and interval for the new format.
##' @param y1 Year of the first date (surveys starting from year y1)
##' @param m1 Month of the first date
##' @param d1 Day of the first date
##' @param y2 Year of the end date (surveys continuing until year y2)
##' @param m2 Month of the end date
##' @param d2 Day of the end date
##' @param every Interval size
##' @param interval Interval size (days, weeks, )
##' @param ... Deprecated arguments
.deployTime_compat <- function(y1 = 2000, y2 = NULL, m1 = 5, m2 = NULL, d1 = 5,
                               d2 = NULL, every = 1, interval = "year", ...) {
  ## Assumptions when missing
  if (is.null(y2)) {
    y2 <- y1
  }
  if (is.null(m2)) {
    m2 <- m1
  }
  if (is.null(d2)) {
    d2 <- d1
  }

  ## Error if non-sensical values are used
  if (y2 < y1) {
    stop("error: y2 < y1")
  }

  o2 <- NULL
  if (interval == "quarter") {
    m1 <- 1
    m2 <- 12
  }

  ## If the start month is before the end month
  if (m1 < m2) {
    for (year in y1:y2) {
      o2 <- c(
        as.Date(o2),
        seq.Date(as.Date(paste(year, m1, d1, sep = "-")),
          as.Date(paste(year, m2, d2, sep = "-")),
          by = paste(every, interval)
        )
      )
    }
  }

  if (m1 >= m2) {
    o2 <- seq.Date(as.Date(paste(y1, m1, d1, sep = "-")),
                   as.Date(paste(y2, m2, d1, sep = "-")),
      by = paste(every, interval)
    )
  }

  ## Return unique dates
  y <- as.Date(sort(unique(o2)))
  return(y)
}
