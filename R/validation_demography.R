##
### Demography

.omuValidation[["demography"]][["name"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
}

.omuValidation[["demography"]][["popSize"]] <- function(popSize, top, coll) {
  checkmate::assert(
    checkmate::checkInteger(
      popSize,
      lower = 1L, upper = 100000L
    ),
    checkmate::checkCharacter(popSize, pattern = "^@.*@"),
    .var.name = top,
    add = coll
  )
}

.omuValidation[["demography"]][["maximumAgeYrs"]] <- function(maximumAgeYrs, top, coll) {
  checkmate::assertDouble(
    maximumAgeYrs, lower = 0, upper = 100, .var.name = top, add = coll
  )
}

## TODO Add growthRate value range; it is not defined in openMalaria's
##      documentation
.omuValidation[["demography"]][["growthRate"]] <- function(growthRate, top, coll) {
  checkmate::assertDouble(growthRate, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["demography"]][["ageGroup"]][["lowerbound"]] <- function(lowerbound, top, coll) {
  checkmate::assertDouble(lowerbound, lower = 0, upper = 100, .var.name = top, add = coll)
}

.omuValidation[["demography"]][["ageGroup"]][["group"]][["poppercent"]] <- function(poppercent, top, coll) {
  checkmate::assertDouble(poppercent, lower = 0, upper = 100, .var.name = top, add = coll)
}

.omuValidation[["demography"]][["ageGroup"]][["group"]][["upperbound"]] <- function(upperbound, top, coll) {
  checkmate::assertDouble(upperbound, lower = 0, upper = 100, .var.name = top, add = coll)
}
