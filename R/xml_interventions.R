##' @title Make sure required elements are declared
##' @param baseList List with experiment data.
##' @param name Name of the interventions.
##' @keywords internal
.defineInterventionsHeader <- function(baseList, name = "All interventions") {
  ## Verify input
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertList(baseList, add = assertCol)
  checkmate::assertCharacter(name, add = assertCol)
  checkmate::reportAssertions(assertCol)

  baseList <- .xmlAddList(
    data = baseList,
    sublist = "interventions",
    entry = "name",
    input = name,
    append = FALSE
  )

  return(baseList)
}
