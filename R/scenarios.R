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
generateScenarios <- function(baseFile = omuCache$baseXml,
                              prefix = omuCache$experimentName,
                              scenarios = NULL, full = NULL, rowStart = NULL, rowEnd = NULL) {
  ## Input validation
  print(omuCache$baseXml)
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
      from = omuCache$baseXml,
      to = file.path(
        omuCache$scenariosDir,
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
    sapply(omuCache$placeholders, function(x) {
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
      if (!(x %in% omuCache$placeholders)) {
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
          omuCache$scenariosDir,
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
      file = file.path(omuCache$cacheDir, "scens.RData")
    )
  }
}
