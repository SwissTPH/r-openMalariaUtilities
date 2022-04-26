### Generate the base xml file

## The base xml file is an openMalaria xml file but it can contain
## @placeholders@ which are used to generate the various scenarios. If no
## placeholders are used, the base xml file is the actual scenario file. The
## input data is a list having a distinct structure resembling the openMalaria
## xml files. Attribute and element names should be the same as in openMalaria.

##' @title Create a base xml file and folder structure
##' @description Processes a list as containing the required information to
##'   generate a base xml file for OpenMalaria. This file is used to generate
##'   the scenarios for simulation.
##' @details The 'data' argument is a nested list resembling the input xml of
##'   OpenMalaria. The attribute names and possible values can be found in
##'   OpenMalaria's schema documentation. Some attributes are not part of the
##'   official schema, like 'expName'. More details can be found in the
##'   vignette.
##'   The function also creates the folder structure for the experiment. By
##'   default, the experiment is created in the current working directory. This
##'   behavior can be changed by modifying or setting 'rootDir', 'scenariosDir'
##'   and 'logsDir' in the input list.
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
  assign(x = "OMVersion", value = data[["OMVersion"]], envir = .pkgcache)

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

##' @rdname createBaseXml
##' @export
create_base_xml <- createBaseXml


##' @title Download required Open Malaria files
##' @param version Major schema version. Supported: 44.
##' @param dir Target directory. Defaults to experiment directory.
##' @export
setupOM <- function(version = 44, dir = NULL) {
  ## Check for supported version and select correct subversion
  suppVers <- c(44)
  if (version %in% suppVers) {
    major <- version
    ## Assigning supported minor version. Should always be the latest.
    version <- ifelse(version == 44, "44.0")

    ## Download files into experiment folder if not already present
    if (is.null(dir)) {
      dir <- get("experimentDir", envir = .pkgcache)
    }

    ## Utility files
    for (f in c("autoRegressionParameters.csv", "densities.csv")) {
      if (!file.exists(file.path(dir, f))) {
        utils::download.file(
          url = paste0(
            "https://raw.githubusercontent.com/SwissTPH/openmalaria/schema-",
            version,
            "/test/", f
          ),
          destfile = file.path(dir, f)
        )
      } else {
        message(paste0("File ", f, " already exists, skipping."))
      }
    }

    ## Schema file
    f <- paste0("scenario_", major, ".xsd")
    if (!file.exists(file.path(dir, f))) {
      utils::download.file(
        url = paste0(
          "https://raw.githubusercontent.com/SwissTPH/openmalaria/schema-",
          version,
          "/schema/", f
        ),
        destfile = file.path(dir, f)
      )
    } else {
      message(paste0("File ", f, " already exists, skipping."))
    }
  } else {
    stop(paste0(
      "Only the following major versions of Open Malaria are supported: ",
      suppVers
    ))
  }
  
  ## Data type validation of baseXml against schema
  baseXml <- xml2::read_xml(get(x = "baseXml", envir = .pkgcache))
  schema <- xml2::read_xml(file.path(dir, f))
  stdout <- attr(xml2::xml_validate(baseXml, schema),"errors")
  errors_other_than_placeholders <- stdout[!grepl("@(.*?)@",stdout)]
  if(length(errors_other_than_placeholders)>0){
    warning(paste0("Validation against schema ",f," failed:"))
    warning(errors_other_than_placeholders)
  }
  
}

##' @rdname setupOM
##' @export
setup_om <- setupOM
