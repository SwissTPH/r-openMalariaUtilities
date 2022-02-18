##' @title Generate placeholder sequence
##' @param x Input data. It is assumed that this is a list is in the form of
##'   'list(foo = list("bar", c(1, 2, 3, ...)), ...)' or 'list(foo = c(seq1,
##'   seq2, ...), ...)'
##' @param placeholders Character vector of names which should be processed
##' @keywords internal
.placeholderseqGen <- function(x, placeholders) {
  ## We loop over the input list, checking if an entry is not in 'placeholders'
  ## and is a list. If yes, then we enclose the name in '@'s and append the
  ## values from the sequence.
  placeholderseq <- list()
  for (arg in names(x)) {
    if (arg %in% placeholders) {
      if (is.list(x[[arg]])) {
        placeholderseq[[arg]] <- paste0("@", x[[arg]][[1]], x[[arg]][[2]], "@")
      }
      if (is.vector(x[[arg]], mode = "character")) {
        matched <- grepl("@(.*?)@", x[[arg]])
        placeholderseq[[arg]] <- x[[arg]][matched]
      }
    }
  }
  ## Make sure that there are no empty entries
  for (arg in names(placeholderseq)) {
    if (!length(placeholderseq[[arg]]) > 0) {
      placeholderseq[[arg]] <- NULL
    }
  }
  return(placeholderseq)
}


##' @title Equalize length of placeholder sequences
##' @param x Sequence which should be the maximum length.
##' @param placeholderseq List with placeholder sequences.
##' @keywords internal
.equalizePlaceholders <- function(x, placeholderseq) {
  ## Find the maximum length
  maxlen <- 0
  for (i in names(placeholderseq)) {
    if (length(placeholderseq[[i]]) > maxlen) {
      maxlen <- length(placeholderseq[[i]])
    }
  }

  ## Compare it to x
  if (maxlen > length(x)) {
    stop(paste0(
      "Number of x must be equal or larger than placeholder sequences!\n",
      "Number of x: ", length(x), "\n",
      "Longest placeholder sequence: ", maxlen
    ))
  } else {
    maxlen <- length(x)
  }

  ## Equalize lengths, reuse last value if length needs to be adjusted
  for (var in names(placeholderseq)) {
    entry <- placeholderseq[[var]]
    diffLength <- maxlen - length(entry)
    placeholderseq[[var]] <- append(
      placeholderseq[[var]], rep(entry[length(entry)], diffLength)
    )
  }

  return(placeholderseq)
}

##' TODO cumulative=TRUE && is.null(subpop)
##' @title Writes the deployment of an intervention.
##' @param baseList List with experiment data.
##' @param component Name of intervention.
##' @param cumulative Default is FALSE. Do not set to TRUE.
##' @param effects Either NULL or vector of strings, e.g. c("det","pre","post")
##' @param startDate Date in YYYY-MM-DD format.
##' @param endDate Date in YYYY-MM-DD format.
##' @param interval A string like '1 weeks'. Same as in [seq.Date()]. Or a list
##'   composed of the entries 'days' (optional), 'months' (optional) and
##'   'years'. If a list is used, startDate and endDate are not used and can be
##'   NULL.
##' @param dates If NULL, startDate, endDate and interval are used, else a
##'   vector of dates in YYYY-MM-DD format. Can be a placeholder.
##' @param minAge Minimum age for deployment (used in SMC). Can be a
##'   placeholder.
##' @param maxAge Maximum age for deployment (used in SMC). Can be a
##'   placeholder.
##' @param coverage Value or variable of coverage. Can be a placeholder.
##' @param subpop Either NULL or string. Concatenation component+"-"+subpop will
##'   be id in subpopulation intervention should be restricted to (see
##'   restrictToSubPop in OpenMalaria)
##' @export
deployIT <- function(baseList, component = "ITN", cumulative = FALSE,
                     effects = NULL, startDate = NULL, endDate = NULL,
                     interval, dates = NULL, minAge = NULL, maxAge = NULL,
                     coverage = NULL, subpop = NULL) {

  ## Verify input
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertSubset(cumulative,
    choices = c(TRUE, FALSE),
    add = assertCol
  )
  checkmate::assertVector(subpop,
    null.ok = TRUE,
    len = 1,
    add = assertCol
  )
  checkmate::assertVector(effects,
    null.ok = TRUE,
    add = assertCol
  )
  checkmate::reportAssertions(assertCol)

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
    placeholders = c("component", "dates", "coverage", "minAge", "maxAge")
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


  ## Generate output list
  outlist <- list()
  outlist <- .xmlAddList(
    data = outlist, sublist = NULL,
    entry = NULL,
    input = list(
      name = component
    )
  )

  ## 'component' can have multiple entries, thus if effects is a vector
  ## containing strings, we need to generate one entry for each string.
  ## Component id is concatenation component+'-'+effects[1] etc.
  ## Furthermore, if subpop is not NULL, deployment to subpopulation with
  ## restrictToSubPop id concatenation component+'-'+subpop' is defined
  ## First: is effects NULL?
  if (is.null(effects)) {
    outlist <- append(
      outlist, list(component = list(id = component))
    )
  } else {
    for (eff in effects) {
      outlist <- append(
        outlist, list(component = list(id = paste0(component, "-", eff)))
      )
    }
  }

  ## Use temporary list to add possible entries for subpop and cumulative
  temp <- list()
  ## Second: is subpop NULL?
  if (!is.null(subpop)) {
    temp <- append(temp, list(
      restrictToSubPop = list(
        id = paste0(component, "-", subpop)
      )
    ))
    ## Third: is cumulative TRUE?
    if (cumulative == TRUE) {
      temp <- append(temp, list(
        cumulativeCoverage = list(
          component = paste0(component, "-", subpop)
        )
      ))
    }
  } else {
    ## Third: is cumulative TRUE?
    if (cumulative == TRUE) {
      temp <- append(temp, list(
        cumulativeCoverage = list(
          component = component
        )
      ))
    }
  }

  outlist <- .xmlAddList(
    data = outlist, sublist = NULL,
    entry = "timed",
    input = temp
  )

  ## Add deployments
  for (i in seq_len(length(dates))) {
    temp <- list(
      deploy = list(
        coverage = if (!is.null(placeholderseq[["coverage"]])) {
          placeholderseq[["coverage"]][[i]]
        } else {
          coverage
        },
        time = dates[[i]]
      )
    )

    ## Add minAge and maxAge information if given
    if (!is.null(minAge) && !is.null(maxAge)) {
      temp[["deploy"]][["minAge"]] <- if (!is.null(placeholderseq[["minAge"]])) {
        placeholderseq[["minAge"]][[i]]
      } else {
        minAge
      }
      temp[["deploy"]][["maxAge"]] <- if (!is.null(placeholderseq[["maxAge"]])) {
        placeholderseq[["maxAge"]][[i]]
      } else {
        maxAge
      }
    }

    outlist <- .xmlAddList(
      data = outlist, sublist = c("timed"),
      entry = NULL,
      input = temp
    )
  }

  ## Make sure interventions header is set
  baseList <- .defineInterventionsHeader(baseList = baseList)

  ## Add to base list
  baseList <- .xmlAddList(
    data = baseList, sublist = c("interventions", "human"),
    entry = "deployment", input = outlist
  )

  return(baseList)
}

##' @rdname deployIT
##' @export
deploy_IT <- deployIT

## DEPRECATED
##' @title Writes the deployment of an intervention. Compatibility version.
##' @param baseList List with experiment data.
##' @param component Name of intervention.
##' @param cumulative default is FALSE. Do not set to TRUE.
##' @param effects Either NULL or c("det","pre","post")
##' @param y1 Year of the first date (surveys starting from year y1)
##' @param m1 Month of the first date
##' @param d1 Day of the first date
##' @param y2 Year of the end date (surveys continuing until year y2)
##' @param m2 Month of the end date
##' @param d2 Day of the end date
##' @param every Interval size
##' @param interval Interval size (days, weeks, )
##' @param SIMSTART Starting date of the simulations in the format "yyyy-mm-dd"
##' @param minAge Minimum age for deployment (used in SMC)
##' @param maxAge Maximum age for deployment (used in SMC)
##' @param coverage Value or variable of coverage
##' @param subpop If TRUE, then restricts to a subpopulation (see
##'   restrictToSubPop in OpenMalaria)
##' @param byyear If TRUE, allows coverage to vary by year
##'   ('histITNcov2000',...)
##' @param deployvar Allows for deployment dates to vary (across years y1, ...,
##'   y2)
##' @export
deploy_it_compat <- function(baseList, component = "ITN", cumulative = FALSE,
                             effects = NULL, y1 = 2000, y2 = NULL, m1 = 1,
                             m2 = NULL, d1 = 1, d2 = NULL, every = 1,
                             interval = "month", SIMSTART = "1918-01-01",
                             minAge = NULL, maxAge = NULL, coverage = NULL,
                             byyear = FALSE, deployvar = NULL, subpop = FALSE) {

  ## Translate time information
  if (is.null(deployvar)) {
    dates <- .deployTime_compat(
      y1 = y1, y2 = y2, m1 = m1, m2 = m2, d1 = d1, d2 = d2, every = every,
      interval = interval
    )
    years <- substr(dates, start = 1, stop = 4)
  }

  ## Translate subpop values
  if (subpop == FALSE) {
    subpop <- NULL
  } else {
    if (!is.null(effects)) {
      subpop <- effects[1]
    } else {
      subpop <- component
    }
  }

  ## If deployvar is used, we need to generate date placeholders
  if (!is.null(deployvar) & !is.null(y1) & !is.null(y2)) {
    ## deployvar should be names of deployment variables i.e. "@IRSdeploy2000@",
    ## "@IRSdeploy2001@", "@IRSdeploy2002@", ...
    if (is.null(every)) {
      stop("Specify 'every' (1, for every year)")
    }

    stripped <- gsub(deployvar, pattern = "@", replacement = "")

    ## Could be every year, every 3 years, it all depends on 'every'
    years <- seq(y1, y2, by = every)
    deployvar <- paste0("@", stripped, years, "@")
    dates <- deployvar
  }

  if (is.null(coverage)) {
    if (!byyear) {
      coverage <- paste0("fut", component, "cov")
    }
    if (byyear) {
      coverage <- list(paste0("fut", component, "cov"), years)
    }
  }

  if (!is.null(coverage)) {
    if (!byyear) {
      coverage <- coverage
    }
    if (byyear) {
      coverage <- list(
        gsub(x = coverage, pattern = "@", replacement = ""),
        years
      )
    }
  }

  baseList <- deployIT(
    baseList = baseList, component = component, cumulative = cumulative,
    effects = effects, dates = dates, minAge = minAge, maxAge = maxAge,
    coverage = coverage, subpop = subpop
  )

  return(baseList)
}
