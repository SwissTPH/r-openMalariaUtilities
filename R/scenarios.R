### Generate scenarios from a base xml

## The necessary funcitonality to generate scenarios based on a so called base
## xml file. The base xml file (and the list from which it was generated)
## contains @placeholders@ which should be replaced with new values according to
## the scenario. The scenarios object is a data frame where each row is one
## scenario. Each column is one parameter. Important note: The column names MUST
## match the placeholder name. Otherwise they will not get recognized.


## Load order
##' @include cache.R printing.R
NULL


##' @title Select rows for scenario creation
##' @param scenarios Data frame containing the values for the placeholders. One
##'   row per scenario, placeholders in columns. Column names correspond to the
##'   placeholder names.
##' @param rowStart Starting row. Optional.
##' @param rowEnd End row. Optional.
##' @keywords internal
.scenariosRowSelect <- function(scenarios, rowStart = NULL, rowEnd = NULL) {
  ## Input verification
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(scenarios, add = assertCol)
  checkmate::assertNumber(rowStart, null.ok = TRUE, add = assertCol)
  checkmate::assertNumber(rowEnd, null.ok = TRUE, add = assertCol)
  checkmate::reportAssertions(assertCol)


  if (is.null(rowStart) && is.null(rowEnd)) {
    range <- seq_len(nrow(scenarios))
  } else {
    rowStart <- ifelse(is.null(rowStart), 1, rowStart)
    rowEnd <- ifelse(is.null(rowEnd), nrow(scenarios), rowEnd)
    range <- rowStart:rowEnd
  }
  return(range)
}

##' @title Add a filename column to scenarios
##' @param scenarios Data frame containing the values for the placeholders. One
##'   row per scenario, placeholders in columns. Column names correspond to the
##'   placeholder names.
##' @param prefix Filename prefix
##' @keywords internal
.scenariosFilenames <- function(scenarios, prefix) {
  ## Input verification
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(scenarios, add = assertCol)
  checkmate::assertCharacter(prefix, add = assertCol)
  checkmate::reportAssertions(assertCol)

  ## Store filenames of each scenario in column, if not already present
  if (is.null(scenarios$file)) {
    scenarios$file <- vapply(seq_len(nrow(scenarios)), function(row) {
      filename <- paste(prefix, "_", row, ".xml", sep = "")
      return(filename)
    }, FUN.VALUE = character(1), USE.NAMES = FALSE)
  }
  return(scenarios)
}

##' @title Generate scenario xml files
##' @param scenarios Data frame containing the values for the placeholders. One
##'   row per scenario, placeholders in columns. Column names correspond to the
##'   placeholder names.
##' @param baseFile Compatible base xml file.
##' @param range Row range of scenarios
##' @param placeholders Vector containing the placeholders
##' @param prefix Filename prefix
##' @param ncores Number of parallel processes to use.
##' @keywords internal
.scenariosGenFiles <- function(scenarios, baseFile, range, placeholders,
                               prefix, ncores = 1) {
  ## Input verification
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(scenarios, add = assertCol)
  checkmate::assertCharacter(baseFile, add = assertCol)
  checkmate::assertNumeric(range, add = assertCol)
  checkmate::assertCharacter(placeholders, add = assertCol)
  checkmate::assertCharacter(prefix, add = assertCol)
  checkmate::assertNumber(ncores, add = assertCol)
  checkmate::reportAssertions(assertCol)

  ## If scenarios is NULL, simply copy the base xml file
  if (is.null(scenarios)) {
    file.copy(
      from = baseFile,
      to = file.path(
        getCache(x = "scenariosDir"),
        paste0(getCache(x = "xmlBasename"), ".xml")
      )
    )
  } else {
    ## Read base xml file and loop over rows. Insert value for corresponding
    ## placeholder if found.
    ##
    ## REVIEW Reading the file from disk is kinda stupid. We should have the
    ##        information of the base xml file in memory in list form. Thus we
    ##        should not write to disk, read again and then write to disk again.
    base <- readLines(baseFile)
    ## Check if placeholders in base file are found in scenarios
    tmp <- c()
    for (x in placeholders) {
      if (!(x %in% colnames(scenarios))) {
        tmp <- c(x, tmp)
      }
    }
    if (!is.null(tmp)) {
      stop(
        paste(
          "The following variables are definded in the base xml file but not in the scenarios:\n",
          paste(tmp, collapse = " ")
        )
      )
    }

    ## Generate scenarios
    makeScen <- function(row, scenDir, logpath) {
      tryCatch(
        {
          ## Open new sink connections
          logfile <- file.path(logpath, paste0(prefix, "_", row, ".log"))
          errlogfile <- file.path(
            logpath, paste0(prefix, "_", row, "_error.log")
          )
          zz <- file(logfile, open = "wt")
          zzErr <- file(errlogfile, open = "wt")
          ## Redirect both outputs to a designated file. Unfortunately, R is not
          ## able to split 'message', 'warning', etc. level so have to handle that
          ## oursevles.
          sink(zz, split = TRUE)
          sink(zzErr, type = "message")

          out <- base
          for (var in placeholders) {
            out <- gsub(
              pattern = paste("@", var, "@", sep = ""),
              replacement = scenarios[[var]][[row]],
              x = out
            )
          }
          filename <- paste0(prefix, "_", row, ".xml")

          ## Write file
          cat(out, file = file.path(
            scenDir,
            filename
          ), sep = "\n")
        },
        finally = {
          ## Close sinks
          sink(type = "message")
          sink()
          ## Check if logfiles are empty. If yes, remove them to save a bit
          ## space.
          for (f in c(logfile, errlogfile)) {
            ## If file size = 0 bytes, remove
            ## If the file contains only whitespace, also remove
            if (file.size(f) == 0 || length(processFile(f)) == 0) {
              unlink(f)
            }
          }
        }
      )
    }

    ## Use parallel if ncores > 1
    if (ncores > 1) {
      tryCatch(
        {
          cl <- parallel::makeCluster(ncores - 1, outfile = "")
          invisible(
            parallel::parLapply(
              cl = cl, range, makeScen, scenDir = getCache(x = "scenariosDir"),
              logpath = file.path(getCache(x = "logsDir"), "scenarios")
            )
          )
        },
        finally = {
          parallel::stopCluster(cl)
        }
      )
    } else {
      invisible(
        lapply(
          range, makeScen,
          scenDir = getCache(x = "scenariosDir"),
          logpath = file.path(getCache(x = "logsDir"), "scenarios")
        )
      )
    }
  }
}


## This function should make sure that the scenario data frame is set up
## correctly. Needs to be added: 1. ID 2. file
##' @title Generate scenarios from a base xml file
##' @description Function makes sure that the scenario data frame is set up
##'   correctly. It adds the required 'ID' and 'file' columns.
##' @param x Data frame containing the values for the placeholders for each
##'   scenarios.
##' @export
generateScenarios <- function(x) {
  ## Input validation
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(x, add = assertCol)
  checkmate::reportAssertions(assertCol)

  ## Warn and abort if ID and file column exist already.
  if (any(c("ID", "file") %in% colnames(x))) {
    stop("Data frame contains already a 'file' or 'ID' column. Please remove them beforehand.")
  }

  ## Add file column
  x <- .scenariosFilenames(
    scenarios = x, prefix = getCache(x = "experimentName")
  )
  ## Add ID column
  x <- data.frame(ID = seq(from = 1, to = nrow(x), by = 1), x)
  return(x)
}

##' @rdname generateScenarios
##' @export
generate_scenarios <- generateScenarios

##' @title Generate scenarios from a base xml file
##' @description Function generates scenarios defined in a data frame. In this
##'   data frame each row is a scenario, placeholder values are in the columns.
##'   Column names correspond to the placeholder names.
##' @param baseFile Compatible base xml file.
##' @param prefix Prefix for the scenario files.
##' @param scenarios Data frame containing the values for the placeholders. One
##'   row per scenario, placeholders in columns. Column names correspond to the
##'   placeholder names.
##' @param ncores Number of parallel processes to use.
##' @param rowStart Starting row. Optional.
##' @param rowEnd End row. Optional.
##' @export
setupScenarios <- function(baseFile = NULL, prefix = NULL, scenarios,
                           ncores = 1, rowStart = NULL, rowEnd = NULL) {
  ## Input verification
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(baseFile, null.ok = TRUE, add = assertCol)
  checkmate::assertCharacter(prefix, null.ok = TRUE, add = assertCol)
  checkmate::assertDataFrame(scenarios, add = assertCol)
  checkmate::assertNumber(ncores, add = assertCol)
  checkmate::assertNumber(rowStart, null.ok = TRUE, add = assertCol)
  checkmate::assertNumber(rowEnd, null.ok = TRUE, add = assertCol)
  checkmate::reportAssertions(assertCol)

  ## Get values from cache if not given
  if (is.null(baseFile)) {
    baseFile <- getCache(x = "baseXml")
  }
  if (is.null(prefix)) {
    prefix <- getCache(x = "experimentName")
  }

  ## Read placeholder names, remove '@' signs
  placeholders <- getCache("placeholders")

  ## Use all rows of given scenarios unless rowStart and rowEnd are both given
  range <- .scenariosRowSelect(
    scenarios = scenarios, rowStart = rowStart, rowEnd = rowEnd
  )

  .scenariosGenFiles(
    scenarios = scenarios, baseFile = baseFile, range = range, ncores = ncores,
    placeholders = placeholders, prefix = prefix
  )
}

##' @rdname setupScenarios
##' @export
setup_scenarios <- setupScenarios

##' @title Store scenarios in cache folder
##' @param scenarios Data frame containing the values for the placeholders. One
##'   row per scenario, placeholders in columns. Column names correspond to the
##'   placeholder names.
##' @param csv Additionally save scenarios as .csv file.
##' @export
storeScenarios <- function(scenarios, csv = TRUE) {
  ## Input verification
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(scenarios, add = assertCol)
  checkmate::assertLogical(csv, add = assertCol)
  checkmate::reportAssertions(assertCol)

  ## Write csv if requested
  if (csv == TRUE) {
    utils::write.csv(
      x = scenarios,
      file = file.path(getCache("experimentDir"), "scenarios.csv"),
      row.names = FALSE
    )
  }

  ## Save RData file
  saveRDS(
    scenarios,
    file = file.path(getCache(x = "cacheDir"), "scenarios.rds")
  )
}

##' @rdname storeScenarios
##' @export
store_scenarios <- storeScenarios

##' @title Return cached scenarios
##' @param experimentDir Directory of the experiment
##' @export
readScenarios <- function(experimentDir = NULL) {
  ## Input verification
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(experimentDir, null.ok = TRUE, add = assertCol)
  checkmate::reportAssertions(assertCol)

  ## Try to get the experimentDir from cache if not given as input
  if (is.null(experimentDir)) {
    experimentDir <- getCache("experimentDir")
  }

  ## Read RDS file
  scenarios <- readRDS(
    file = file.path(experimentDir, "cache", "scenarios.rds")
  )

  return(scenarios)
}

##' @rdname readScenarios
##' @export
read_scenarios <- readScenarios
