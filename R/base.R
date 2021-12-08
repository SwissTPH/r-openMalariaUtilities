### Generate the base xml file

## The base xml file is an openMalaria xml file but it can contain
## @placeholders@ which are used to generate the various scenarios. If no
## placeholders are used, the base xml file is the actual scenario file. The
## input data is a list having a distinct structure resembling the openMalaria
## xml files. Attribute and element names should be the same as in openMalaria.

## TODO Document the list structure used, esp. the options NOT part of the
## openMalaria spec

##' Processes a list as containing all information to generate all required
##' folders and files for openMalaria. Spaces in the name of the experiment are
##' automatically replaced by underscores.
##'
##' @title Create an openMalaria experiment
##' @param data List containing all information
##' @param replace Overwrite experiment directory if it is already present.
##'   Possible values are TRUE, FALSE, or "ask".
##' @return
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
