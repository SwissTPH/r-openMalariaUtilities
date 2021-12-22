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
