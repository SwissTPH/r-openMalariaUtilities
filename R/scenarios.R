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

##' @title Generate scenarios from a base xml file and a scenarios data frame
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
    baseFile <- .omupkgcache$baseXml
  }
  if (is.null(prefix)) {
    prefix <- .omupkgcache$experimentName
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
  if (is.null(rowStart) | is.null(rowEnd)) {
    range <- seq_len(nrow(scenarios))
  } else {
    range <- rowStart:rowEnd
  }

  ## If scenarios and full are NULL, simply copy the base xml file
  if (is.null(scenarios) & is.null(full)) {
    file.copy(
      from = .omupkgcache$baseXml,
      to = file.path(
        .omupkgcache$scenariosDir,
        paste0(xmlBasename, ".xml")
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
    tmp <- NULL
    sapply(.omupkgcache$placeholders, function(x) {
      if (!(x %in% placeholders)) {
        tmp <<- c(x, tmp)
      }
    })
    if (!is.null(tmp)) {
      stop(paste("The following variables are definded in the base xml file but not in the scenarios:\n", tmp))
    }
    ## Check if scenarios has more placeholders than used in the base file
    tmp <- NULL
    sapply(placeholders, function(x) {
      if (!(x %in% .omupkgcache$placeholders)) {
        tmp <<- c(x, tmp)
      }
    })
    if (!is.null(tmp)) {
      warning(paste("The following variables are not used in the base xml file but definded in the scenarios:\n", tmp))
    }
    ## Prepare column to store filenames
    scenarios$file <- NA
    invisible(
      sapply(range, function(row) {
        out <- base
        sapply(placeholders, function(var) {
          out <<- gsub(
            pattern = paste("@", var, "@", sep = ""),
            replacement = scenarios[[var]][[row]],
            x = out
          )
        })
        filename <- file.path(
          .omupkgcache$scenariosDir,
          paste(prefix, "_", row, ".xml", sep = "")
        )
        ## Store filename
        scenarios[row, ]$file <- filename
        ## Write file
        cat(out, file = filename, sep = "\n")
      })
    )
    ## Cache scenarios
    ## Compatibility
    scens <- scenarios
    save(scenarios, full, scens,
      file = file.path(.omupkgcache$cacheDir, "scens.RData")
    )
  }
}
