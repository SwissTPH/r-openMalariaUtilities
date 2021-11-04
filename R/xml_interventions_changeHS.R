### Little helpers for the the interventions/changeHS section

##' @title TODO
##' @param interpolation TODO
##' @param ageGroups TODO
##' @return TODO
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

##' @title TODO
##' @param interpolation TODO
##' @param ageGroups TODO
##' @return TODO
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
