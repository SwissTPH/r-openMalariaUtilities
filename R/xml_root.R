##
### Create document root

.makeXmlRoot <- function(schemaVersion, name, analysisNo, validate = TRUE) {
  ## Input verification
  if (validate == TRUE) {
    assertCol <- checkmate::makeAssertCollection()
    checkmate::assertInteger(schemaVersion, add = assertCol)
    checkmate::assertCharacter(name, add = assertCol)
    checkmate::assertInteger(
      analysisNo,
      lower = 1L, upper = 100000000L, null.ok = TRUE, add = assertCol
    )
    checkmate::reportAssertions(collection = assertCol)
  }
  ## Create root
  scenario <- xml2::xml_new_root(
    "om:scenario",
    "xmlns:om" = paste0(
      "http://openmalaria.org/schema/scenario_",
      schemaVersion
    ),
    "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
    "name" = name,
    "schemaVersion" = schemaVersion,
    "xsi:schemaLocation" = paste0(
      "http://openmalaria.org/schema/scenario_",
      schemaVersion,
      " ", "scenario_", schemaVersion, ".xsd"
    )
  )
  ## Add optional scenario attributes
  xml2::xml_set_attr(
    scenario,
    "analysisNo", analysisNo
  )
  ## Return root scenario
  return(scenario)
}
