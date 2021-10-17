##
### Monitoring

.omuValidation[["monitoring"]][["name"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
}

.omuValidation[["monitoring"]][["startDate"]] <- function(startDate, top, coll) {
  checkmate::assertCharacter(
    startDate,
    pattern = "^\\d{4}\\-\\d{2}\\-\\d{2}", .var.name = top, add = coll
  )
}

.omuValidation[["monitoring"]][["continous"]][["period"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
}

## TODO Not sure what value that should be?
##      https://swisstph.github.io/openmalaria/schema-43.html#during-initialization
## .omuValidation[["monitoring"]][["continous"]][["duringInit"]] <- function(name, top, coll) {
##   checkmate::assertCharacter(name, .var.name = top, add = coll)
## }

.omuValidation[["monitoring"]][["continous"]][["option"]][["name"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
}

.omuValidation[["monitoring"]][["continous"]][["option"]][["value"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, null.ok = TRUE, .var.name = top, add = coll)
  checkmate::assertSubset(
    name,
    choices = c("true", "false", NA),
    .var.name = top,
    add = coll
  )
}

.omuValidation[["monitoring"]][["SurveyOptions"]][["onlyNewEpisode"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, null.ok = TRUE, .var.name = top, add = coll)
  checkmate::assertSubset(
    name,
    choices = c("true", "false", NA),
    .var.name = top,
    add = coll
  )
}

.omuValidation[["monitoring"]][["SurveyOptions"]][["option"]][["name"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
}

.omuValidation[["monitoring"]][["SurveyOptions"]][["option"]][["value"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, null.ok = TRUE, .var.name = top, add = coll)
  checkmate::assertSubset(
    name,
    choices = c("true", "false", NA),
    .var.name = top,
    add = coll
  )
}

.omuValidation[["monitoring"]][["SurveyOptions"]][["option"]][["outputNumber"]] <- function(name, top, coll) {
  checkmate::assertInteger(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["monitoring"]][["SurveyOptions"]][["option"]][["byAge"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, null.ok = TRUE, .var.name = top, add = coll)
  checkmate::assertSubset(
    name,
    choices = c("true", "false", NA),
    .var.name = top,
    add = coll
  )
}

.omuValidation[["monitoring"]][["SurveyOptions"]][["option"]][["byCohort"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, null.ok = TRUE, .var.name = top, add = coll)
  checkmate::assertSubset(
    name,
    choices = c("true", "false", NA),
    .var.name = top,
    add = coll
  )
}

.omuValidation[["monitoring"]][["SurveyOptions"]][["option"]][["bySpecies"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, null.ok = TRUE, .var.name = top, add = coll)
  checkmate::assertSubset(
    name,
    choices = c("true", "false", NA),
    .var.name = top,
    add = coll
  )
}

.omuValidation[["monitoring"]][["SurveyOptions"]][["option"]][["byGenotype"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, null.ok = TRUE, .var.name = top, add = coll)
  checkmate::assertSubset(
    name,
    choices = c("true", "false", NA),
    .var.name = top,
    add = coll
  )
}

.omuValidation[["monitoring"]][["SurveyOptions"]][["option"]][["byDrugType"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, null.ok = TRUE, .var.name = top, add = coll)
  checkmate::assertSubset(
    name,
    choices = c("true", "false", NA),
    .var.name = top,
    add = coll
  )
}

.omuValidation[["monitoring"]][["surveys"]][["detectionLimit"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, lower = 0, null.ok = TRUE, .var.name = top, add = coll)
  ## DEPRECATED detectionLimit should be dropped in favor of diagnostic
  if (!is.null(name)) {
    warning("The use of monitoring/surveys/detectionLimit has been deprecated.
See https://swisstph.github.io/openmalaria/schema-33.html#detection-limit-for-parasitaemia.", call. = FALSE)
  }
}

.omuValidation[["monitoring"]][["surveys"]][["diagnostic"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["monitoring"]][["surveys"]][["surveyTime"]][["repeatStep"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["monitoring"]][["surveys"]][["surveyTime"]][["repeatEnd"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["monitoring"]][["surveys"]][["surveyTime"]][["reported"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, null.ok = TRUE, .var.name = top, add = coll)
  checkmate::assertSubset(
    name,
    choices = c("true", "false", NA),
    .var.name = top,
    add = coll
  )
}

.omuValidation[["monitoring"]][["ageGroup"]][["lowerbound"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, lower = 0, upper = 100, .var.name = top, add = coll)
}

.omuValidation[["monitoring"]][["ageGroup"]][["group"]][["upperbound"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, lower = 0, upper = 100, .var.name = top, add = coll)
}

.omuValidation[["monitoring"]][["cohorts"]][["subPop"]][["id"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
}

.omuValidation[["monitoring"]][["cohorts"]][["subPop"]][["number"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, lower = 1, upper = 2097152, .var.name = top, add = coll)
}
