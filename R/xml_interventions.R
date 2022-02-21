##' @title Make sure required elements are declared
##' @param baseList List with experiment data.
##' @param name Name of the interventions.
##' @keywords internal
.defineInterventionsHeader <- function(baseList, name = "All interventions") {
  baseList <- .xmlAddList(
    data = baseList,
    sublist = "interventions",
    entry = "name",
    input = name,
    append = FALSE
  )

  return(baseList)
}
