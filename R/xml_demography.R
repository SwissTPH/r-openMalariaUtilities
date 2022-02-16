### Little helpers for the the demography section

##' @title Generate list for 'demography/ageGroup'
##' @param lowerbound Double, lower bound of age group
##' @param ageGroups Data frame containing demography
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
  ageGroups[c("poppercent", "upperbound")] <- lapply(
    ageGroups[c("poppercent", "upperbound")], as.double
  )
  outlist <- .xmlAddChunks(
    outlist = outlist, element = "group", attributeList = ageGroups
  )
  return(outlist)
}

## See https://swisstph.github.io/openmalaria/schema-43.html#elt-demography

##' @title Define and write demography input to baseList
##' @param baseList List with experiment data.
##' @param name Name of demography data.
##' @param popSize Population size.
##' @param maximumAgeYrs Maximum age of simulated humans in years.
##' @param growthRate Growth rate of human population.
##' @param lowerbound Lower bound of age group.
##' @param upperbound Upper bound of age group. Must be a numerical vector.
##' @param poppercent Percentage of human population in age group. Must be a
##'   numerical vector.
##' @export
defineDemography <- function(baseList, name, popSize = 3000,
                             maximumAgeYrs, growthRate = NULL, lowerbound,
                             poppercent, upperbound) {
  ## Input validation
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(name, add = assertCol)
  checkmate::assert(
    checkmate::checkInteger(popSize, lower = 1L, upper = 100000L),
    checkmate::checkCharacter(popSize, pattern = "@(.*?)@"),
    add = assertCol
  )
  checkmate::assert(
    checkmate::checkDouble(maximumAgeYrs, lower = 0, upper = 100),
    checkmate::checkCharacter(maximumAgeYrs, pattern = "@(.*?)@"),
    add = assertCol
  )
  checkmate::assert(
    checkmate::checkDouble(growthRate, null.ok = TRUE),
    checkmate::checkCharacter(growthRate, pattern = "@(.*?)@"),
    add = assertCol
  )
  checkmate::assert(
    checkmate::checkDouble(lowerbound, lower = 0, upper = 100),
    checkmate::checkCharacter(lowerbound, pattern = "@(.*?)@"),
    add = assertCol
  )
  checkmate::assertDouble(upperbound, lower = 0, upper = 100, add = assertCol)
  checkmate::assertDouble(poppercent, lower = 0, upper = 100, add = assertCol)
  checkmate::reportAssertions(collection = assertCol)

  ## Assign values to output list
  baseList <- .xmlAddList(
    data = baseList, sublist = NULL, append = FALSE, entry = "demography",
    input = c(
      list(
        name = name,
        popSize = popSize,
        maximumAgeYrs = maximumAgeYrs
      ),
      if (!is.null(growthRate)) {
        list(growthRate = growthRate)
      },
      list(
        ageGroup = ageGroupsGen(
          lowerbound = lowerbound,
          ageGroups = data.frame(
            poppercent = poppercent,
            upperbound = upperbound
          )
        )
      )
    )
  )

  return(baseList)
}

##' @rdname defineDemography
##' @export
define_demography <- defineDemography
