## TODO Extend information
## data should be a list containing all settings and interventions which should
## be merged into the xml

##' Processes a list as containing all information to generate all required
##' folders and files for openMalaria. Spaces in the name of the experiment are
##' automatically replaced by underscores.
##'
##' @title Create an openMalaria experiment
##' @param data List containing all information
##' @param validate Boolean, if data should be validated
##' @return
##' @export
createBaseXml <- function(data = NULL, validate = TRUE) {
  ## Replace spaces with underscores in experiment name and cache it
  data[["expName"]] <- gsub(" ", "_", data[["expName"]])
  omuCache$experimentName <- data[["expName"]]
  ## Variables
  if (is.null(data[["xmlBasename"]])) {
    xmlBasename <- paste0(omuCache$experimentName, "_base")
  } else {
    xmlBasename <- data[["xmlBasename"]]
  }
  ## Generate document root
  baseXml <- .makeXmlRoot(
    schemaVersion = data[["OMVersion"]],
    name = omuCache$experimentName,
    analysisNo = data[["analysisNo"]],
    validate = validate
  )
  ## Construct xml document
  .xmlMakeDocRec(baseXML = baseXml, data = data, validate = validate)
  ## Create folders
  .createFolders(
    experimentName = omuCache$experimentName,
    rootDir = data[["rootDir"]],
    scenariosDir = data[["scenariosDir"]],
    logsDir = data[["logsDir"]],
    replace = "ask"
  )
  omuCache$baseXml <- file.path(
    omuCache$experimentDir,
    paste0(xmlBasename, ".xml")
  )
  ## Write base xml file
  xml2::write_xml(baseXml, file = omuCache$baseXml)
  ## Write cache
  .storeCache()
}
