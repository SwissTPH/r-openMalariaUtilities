### Generate scenarios from a base xml

## The necessary funcitonality to generate scenarios based on a so called base
## xml file. The base xml file (and the list from which it was generated)
## contains @placeholders@ which should be replaced with new values according to
## the scenario. The scenarios object is a data frame where each row is one
## scenario. Each column is one parameter. Important note: The column names MUST
## match the placeholder name without the @ symbols. Otherwise they will not get
## recognized.
##
## The object full is a starting point from the experiments (full factorial).

## REVIEW Is keeping full really necessary? Can it be factored out?

##' @title Store scenarios and full object in cache folder
##' @param scenarios Data frame containing the values for the placeholders. One
##'   row per scenario, placeholders in columns. Column names correspond to the
##'   placeholder names.
##' @param full List of experiment variables and values.
##' @export
storeScenarios <- function(scenarios, full) {
  ## Compatibility
  scens <- scenarios
  save(scenarios, full, scens,
    file = file.path(get(x = "cacheDir", envir = .pkgcache), "scens.RData")
  )
}

##' @title Select rows for scenario creation
##' @param scenarios Data frame containing the values for the placeholders. One
##'   row per scenario, placeholders in columns. Column names correspond to the
##'   placeholder names.
##' @param rowStart Starting row. Optional.
##' @param rowEnd End row. Optional.
##' @keywords internal
.scenariosRowSelect <- function(scenarios, rowStart = NULL, rowEnd = NULL) {
  if (is.null(rowStart) | is.null(rowEnd)) {
    range <- seq_len(nrow(scenarios))
  } else {
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
  ## Store filenames of each scenario in column
  scenarios$file <- vapply(seq_len(nrow(scenarios)), function(row) {
    filename <- paste(prefix, "_", row, ".xml", sep = "")
    return(filename)
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)
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
##' @keywords internal
.scenariosGenFiles <- function(scenarios, baseFile, range, placeholders,
                               prefix) {
  ## If scenarios and full are NULL, simply copy the base xml file
  if (is.null(scenarios)) {
    file.copy(
      from = baseFile,
      to = file.path(
        get(x = "scenariosDir", envir = .pkgcache),
        paste0(get(x = "xmlBasename", envir = .pkgcache), ".xml")
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
    for (x in get(x = "placeholders", envir = .pkgcache)) {
      if (!(x %in% placeholders)) {
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

    ## Check if scenarios has more placeholders than used in the base file
    tmp <- c()
    for (x in placeholders) {
      if (!(x %in% get(x = "placeholders", envir = .pkgcache))) {
        tmp <- c(x, tmp)
      }
    }
    if (!is.null(tmp)) {
      warning(
        paste(
          "The following variables are not used in the base xml file but definded in the scenarios:\n",
          paste(tmp, collapse = " ")
        )
      )
    }

    ## Generate scenarios
    lapply(range, function(row) {
      out <- base
      for (var in placeholders) {
        out <- gsub(
          pattern = paste("@", var, "@", sep = ""),
          replacement = scenarios[[var]][[row]],
          x = out
        )
      }
      filename <- paste(prefix, "_", row, ".xml", sep = "")
      ## Write file
      cat(out, file = file.path(
        get(x = "scenariosDir", envir = .pkgcache),
        filename
      ))
    })
  }
}


##' @title Generate scenarios from a base xml file
##' @description Function generates scenarios defined in a data frame. In this
##'   data frame each row is a scenario, placeholder values are in the columns.
##'   Column names correspond to the placeholder names.
##' @param baseFile Compatible base xml file.
##' @param prefix Prefix for the scenario files.
##' @param scenarios Data frame containing the values for the placeholders. One
##'   row per scenario, placeholders in columns. Column names correspond to the
##'   placeholder names.
##' @param full List of experiment variables and values.
##' @param rowStart Starting row. Optional.
##' @param rowEnd End row. Optional.
##' @export
generateScenarios <- function(baseFile = NULL, prefix = NULL, scenarios = NULL,
                              full = NULL, rowStart = NULL, rowEnd = NULL) {
  ## Get values from cache if not given
  if (is.null(baseFile)) {
    baseFile <- get(x = "baseXml", envir = .pkgcache)
  }
  if (is.null(prefix)) {
    prefix <- get(x = "experimentName", envir = .pkgcache)
  }

  ## Input validation
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertFileExists(baseFile, add = assertCol)
  checkmate::assertDataFrame(scenarios, add = assertCol)
  checkmate::assertNumber(rowStart, null.ok = TRUE, add = assertCol)
  checkmate::assertNumber(rowEnd, null.ok = TRUE, add = assertCol)
  checkmate::reportAssertions(assertCol)

  ## Read placeholder names
  placeholders <- names(scenarios)
  ## Use all rows of given scenarios unless rowStart and rowEnd are both given
  range <- .scenariosRowSelect(
    scenarios = scenarios, rowStart = rowStart, rowEnd = rowEnd
  )

  ## Store filenames of each scenario in column
  scenarios <- .scenariosFilenames(scenarios = scenarios, prefix = prefix)

  .scenariosGenFiles(
    scenarios = scenarios, baseFile = baseFile, range = range,
    placeholders = placeholders, prefix = prefix
  )

  ## Store scenarios in cache
  ## REVIEW This can get large (100k+ scenarios), maybe a separate cache is
  ## necessary
  assign(x = "scenarios", value = scenarios, envir = .pkgcache)

  ## Cache scenarios
  storeScenarios(
    scenarios = get(x = "scenarios", envir = .pkgcache),
    full = full
  )
}
