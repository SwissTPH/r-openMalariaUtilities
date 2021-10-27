### Little helpers for the the demography section

##' @title Generate list for 'demography/ageGroup'
##' @param lowerbound Double, lower bound of age group
##' @param ageGroups Data frame containing demograpghy
##' @return List for xml contruction
##' @export
ageGroupsGen <- function(lowerbound, ageGroups) {
  ## Input validation
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(ageGroups, add = assertCol)
  checkmate::reportAssertions(collection = assertCol)
  ## Assign lowerbound
  outlist <- list(lowerbound = lowerbound)
  ## Apply expected data types
  ageGroups[c("poppercent", "upperbound")] <- sapply(
    ageGroups[c("poppercent", "upperbound")], as.double
  )
  outlist <- .xmlAddChunks(
    outlist = outlist, element = "group", attributeList = ageGroups
  )
  return(outlist)
}
