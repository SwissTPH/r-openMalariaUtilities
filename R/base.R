### Generate the base xml file

## The base xml file is an openMalaria xml file but it can contain
## @placeholders@ which are used to generate the various scenarios. If no
## placeholders are used, the base xml file is the actual scenario file. The
## input data is a list having a distinct structure resembling the openMalaria
## xml files. Attribute and element names should be the same as in openMalaria.

##' @title Create a base xml file
##' @description Processes a list as containing the required information to
##'   generate a base xml file for OpenMalaria. This file is used to generate
##'   the scenarios for simulation.
##' @details The 'data' argument is a nested list resembling the input xml of
##'   OpenMalaria. The attribute names and possible values can be found in
##'   OpenMalaria's schema documentation. Some attributes are not part of the
##'   official schema, like 'expName'. More details can be found in the
##'   vignette.
##' @param data List containing all information
##' @param replace Overwrite experiment directory if it is already present.
##'   Possible values are TRUE, FALSE, or "ask".
##' @export
createBaseXml <- function(data = NULL, replace = "ask") {
  ## Replace spaces with underscores in experiment name and cache it
  data[["expName"]] <- gsub(" ", "_", data[["expName"]])
  assign(x = "experimentName", value = data[["expName"]], envir = .pkgcache)
  ## Variables
  if (is.null(data[["xmlBasename"]])) {
    xmlBasename <- paste0(get(x = "experimentName", envir = .pkgcache), "_base")
  } else {
    xmlBasename <- data[["xmlBasename"]]
  }
  assign(x = "xmlBasename", value = xmlBasename, envir = .pkgcache)

  ## Generate document root
  baseXml <- .makeXmlRoot(
    schemaVersion = data[["OMVersion"]],
    name = get(x = "experimentName", envir = .pkgcache),
    analysisNo = data[["analysisNo"]]
  )
  ## Construct xml document
  .xmlMakeDocRec(baseXML = baseXml, data = data)
  ## Create folders
  .createFolders(
    experimentName = get(x = "experimentName", envir = .pkgcache),
    rootDir = data[["rootDir"]],
    scenariosDir = data[["scenariosDir"]],
    logsDir = data[["logsDir"]],
    replace = replace
  )
  assign(x = "baseXml", value = file.path(
    get(x = "experimentDir", envir = .pkgcache),
    paste0(xmlBasename, ".xml")
  ), envir = .pkgcache)

  ## Write base xml file
  xml2::write_xml(baseXml, file = get(x = "baseXml", envir = .pkgcache))
  ## Write cache
  .storeCache()
}
