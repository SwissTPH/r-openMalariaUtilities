.omuValidation[["interventions"]][["changeHS"]][["name"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["time"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["name"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["useDiagnosticUC"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, null.ok = TRUE, .var.name = top, add = coll)
  checkmate::assertSubset(
    name,
    choices = c("true", "false", NA),
    .var.name = top,
    add = coll
  )
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["drugRegimen"]][["firstLine"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["drugRegimen"]][["secondLine"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["drugRegimen"]][["inpatient"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["initialACR"]][["CQ"]][["value"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["initialACR"]][["SP"]][["value"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["initialACR"]][["AQ"]][["value"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["initialACR"]][["SPAQ"]][["value"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["initialACR"]][["ACT"]][["value"]] <- function(name, top, coll) {
  checkmate::assert(
    checkmate::checkDouble(name, null.ok = TRUE),
    checkmate::checkCharacter(name, pattern = "^@.*@", null.ok = TRUE),
    .var.name = top, add = coll
  )
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["initialACR"]][["QN"]][["value"]] <- function(name, top, coll) {
  checkmate::assert(
    checkmate::checkDouble(name, null.ok = TRUE),
    checkmate::checkCharacter(name, pattern = "^@.*@", null.ok = TRUE),
    .var.name = top, add = coll
  )
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["initialACR"]][["selfTreatment"]][["value"]] <- function(name, top, coll) {
  checkmate::assert(
    checkmate::checkDouble(name, null.ok = TRUE),
    checkmate::checkCharacter(name, pattern = "^@.*@", null.ok = TRUE),
    .var.name = top, add = coll
  )
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["compliance"]][["CQ"]][["value"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["compliance"]][["SP"]][["value"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["compliance"]][["AQ"]][["value"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["compliance"]][["SPAQ"]][["value"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["compliance"]][["ACT"]][["value"]] <- function(name, top, coll) {
  checkmate::assert(
    checkmate::checkDouble(name, null.ok = TRUE),
    checkmate::checkCharacter(name, pattern = "^@.*@", null.ok = TRUE),
    .var.name = top, add = coll
  )
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["compliance"]][["QN"]][["value"]] <- function(name, top, coll) {
  checkmate::assert(
    checkmate::checkDouble(name, null.ok = TRUE),
    checkmate::checkCharacter(name, pattern = "^@.*@", null.ok = TRUE),
    .var.name = top, add = coll
  )
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["compliance"]][["selfTreatment"]][["value"]] <- function(name, top, coll) {
  checkmate::assert(
    checkmate::checkDouble(name, null.ok = TRUE),
    checkmate::checkCharacter(name, pattern = "^@.*@", null.ok = TRUE),
    .var.name = top, add = coll
  )
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["nonCompliersEffective"]][["CQ"]][["value"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["nonCompliersEffective"]][["SP"]][["value"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["nonCompliersEffective"]][["AQ"]][["value"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["nonCompliersEffective"]][["SPAQ"]][["value"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["nonCompliersEffective"]][["ACT"]][["value"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["nonCompliersEffective"]][["QN"]][["value"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["nonCompliersEffective"]][["selfTreatment"]][["value"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["CQ"]][["name"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["CQ"]][["deploy"]][["maxAge"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, lower = 0, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["CQ"]][["deploy"]][["minAge"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, lower = 0, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["CQ"]][["deploy"]][["p"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, lower = 0, upper = 1, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["CQ"]][["deploy"]][["component"]][["id"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["CQ"]][["clearInfections"]][["timesteps"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["CQ"]][["clearInfections"]][["stage"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
  checkmate::assertSubset(
    name,
    choices = c("liver", "blood", "both"),
    .var.name = top,
    add = coll
  )
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["SP"]][["name"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["SP"]][["deploy"]][["maxAge"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, lower = 0, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["SP"]][["deploy"]][["minAge"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, lower = 0, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["SP"]][["deploy"]][["p"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, lower = 0, upper = 1, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["SP"]][["deploy"]][["component"]][["id"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["SP"]][["clearInfections"]][["timesteps"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["SP"]][["clearInfections"]][["stage"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
  checkmate::assertSubset(
    name,
    choices = c("liver", "blood", "both"),
    .var.name = top,
    add = coll
  )
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["AQ"]][["name"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["AQ"]][["deploy"]][["maxAge"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, lower = 0, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["AQ"]][["deploy"]][["minAge"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, lower = 0, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["AQ"]][["deploy"]][["p"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, lower = 0, upper = 1, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["AQ"]][["deploy"]][["component"]][["id"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["AQ"]][["clearInfections"]][["timesteps"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["AQ"]][["clearInfections"]][["stage"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
  checkmate::assertSubset(
    name,
    choices = c("liver", "blood", "both"),
    .var.name = top,
    add = coll
  )
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["SPAQ"]][["name"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["SPAQ"]][["deploy"]][["maxAge"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, lower = 0, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["SPAQ"]][["deploy"]][["minAge"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, lower = 0, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["SPAQ"]][["deploy"]][["p"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, lower = 0, upper = 1, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["SPAQ"]][["deploy"]][["component"]][["id"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["SPAQ"]][["clearInfections"]][["timesteps"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["SPAQ"]][["clearInfections"]][["stage"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
  checkmate::assertSubset(
    name,
    choices = c("liver", "blood", "both"),
    .var.name = top,
    add = coll
  )
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["ACT"]][["name"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["ACT"]][["deploy"]][["maxAge"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, lower = 0, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["ACT"]][["deploy"]][["minAge"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, lower = 0, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["ACT"]][["deploy"]][["p"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, lower = 0, upper = 1, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["ACT"]][["deploy"]][["component"]][["id"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["ACT"]][["clearInfections"]][["timesteps"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["ACT"]][["clearInfections"]][["stage"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
  checkmate::assertSubset(
    name,
    choices = c("liver", "blood", "both"),
    .var.name = top,
    add = coll
  )
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["QN"]][["name"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["QN"]][["deploy"]][["maxAge"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, lower = 0, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["QN"]][["deploy"]][["minAge"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, lower = 0, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["QN"]][["deploy"]][["p"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, lower = 0, upper = 1, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["QN"]][["deploy"]][["component"]][["id"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["QN"]][["clearInfections"]][["timesteps"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["treatmentActions"]][["QN"]][["clearInfections"]][["stage"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, .var.name = top, add = coll)
  checkmate::assertSubset(
    name,
    choices = c("liver", "blood", "both"),
    .var.name = top,
    add = coll
  )
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["pSeekOfficialCareUncomplicated1"]][["value"]] <- function(name, top, coll) {
  checkmate::assert(
    checkmate::checkDouble(name, lower = 0.0, upper = 1.0, null.ok = TRUE),
    checkmate::checkCharacter(name, pattern = "^@.*@", null.ok = TRUE),
    .var.name = top, add = coll
  )
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["pSelfTreatUncomplicated"]][["value"]] <- function(name, top, coll) {
    checkmate::assert(
    checkmate::checkDouble(name, lower = 0.0, upper = 1.0, null.ok = TRUE),
    checkmate::checkCharacter(name, pattern = "^@.*@", null.ok = TRUE),
    .var.name = top, add = coll
  )
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["pSeekOfficialCareUncomplicated2"]][["value"]] <- function(name, top, coll) {
  checkmate::assert(
    checkmate::checkDouble(name, lower = 0.0, upper = 1.0, null.ok = TRUE),
    checkmate::checkCharacter(name, pattern = "^@.*@", null.ok = TRUE),
    .var.name = top, add = coll
  )
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["ImmediateOutcomes"]][["pSeekOfficialCareSevere"]][["value"]] <- function(name, top, coll) {
  checkmate::assert(
    checkmate::checkDouble(name, lower = 0.0, upper = 1.0, null.ok = TRUE),
    checkmate::checkCharacter(name, pattern = "^@.*@", null.ok = TRUE),
    .var.name = top, add = coll
  )
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["CFR"]][["interpolation"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, null.ok = TRUE, .var.name = top, add = coll)
  checkmate::assertSubset(
    name,
    choices = c("none", "linear"),
    .var.name = top,
    add = coll
  )
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["CFR"]][["group"]][["value"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["CFR"]][["group"]][["lowerbound"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, lower = 0, upper = 100, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["pSequelaeInpatient"]][["interpolation"]] <- function(name, top, coll) {
  checkmate::assertCharacter(name, null.ok = TRUE, .var.name = top, add = coll)
  checkmate::assertSubset(
    name,
    choices = c("none", "linear"),
    .var.name = top,
    add = coll
  )
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["pSequelaeInpatient"]][["group"]][["value"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, null.ok = TRUE, .var.name = top, add = coll)
}

.omuValidation[["interventions"]][["changeHS"]][["timedDeployment"]][["pSequelaeInpatient"]][["group"]][["lowerbound"]] <- function(name, top, coll) {
  checkmate::assertDouble(name, lower = 0, upper = 100, null.ok = TRUE, .var.name = top, add = coll)
}
