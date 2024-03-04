### Generate the base xml file

## The base xml file is an openMalaria xml file but it can contain
## @placeholders@ which are used to generate the various scenarios. If no
## placeholders are used, the base xml file is the actual scenario file. The
## input data is a list having a distinct structure resembling the openMalaria
## xml files. Attribute and element names should be the same as in openMalaria.

##' @include cache.R printing.R
NULL

.thirdDimensionGen <- function() {
  ## Check which features are enabled; Age groups are by default
  isCohorts <- ifelse(
    "mon_cohorts" %in% ls(all.names = TRUE, envir = .pkgcache), TRUE, FALSE
  )

  ## Creat thirdDimension entry for age groups
  ageGroups <- getCache("mon_ageGroups")
  thirdDimension <- data.table::data.table(
    number = seq_len(length(ageGroups$upperbounds)),
    id = {
      out <- c(paste0(ageGroups$lowerbound, "-", ageGroups$upperbounds[1]))
      for (i in seq_len(length(ageGroups$upperbounds))[-length(ageGroups$upperbounds)]) {
        out <- c(out, paste0(ageGroups$upperbounds[i], "-", ageGroups$upperbounds[i + 1]))
      }
      out
    }
  )

  if (isCohorts) {
    cohorts <- getCache("mon_cohorts")
    cohortNum <- cohorts$number
    cohortID <- cohorts$id
    cohorts <- data.table::data.table(
      number = rapply(
        sapply(
          1:length(cohortNum),
          function(n) utils::combn(cohortNum, n, simplify = FALSE)
        ), sum
      ),
      id = rapply(
        sapply(
          1:length(cohortID),
          function(n) utils::combn(cohortID, n, simplify = FALSE)
        ), paste0,
        collapse = ""
      )
    )

    ## Create updated thirdDimension table
    cohorts$number <- cohorts$number * 1000

    tmp <- list()
    for (i in seq_len(nrow(cohorts))) {
      tmp[[i]] <- list(
        number = cohorts[[i, "number"]] + thirdDimension[["number"]],
        id = paste0(cohorts[[i, "id"]], ":", thirdDimension[["id"]])
      )
    }
    tmp <- c(tmp, list(list(
      number = thirdDimension[["number"]],
      id = thirdDimension[["id"]]
    )))
    thirdDimension <- data.table::rbindlist(tmp)
  }

  ## Add the special number 0, which corresponds to id = "none". This is
  ## reserved for measures like simulatedEIR, which are not segregated by age
  ## group or similar.
  thirdDimension <- data.table::rbindlist(
    list(thirdDimension, list(number = 0, id = "none")))

  return(thirdDimension)
}


##' @title Create a base XML file and folder structure
##' @description Processes a list as containing the required information to
##'   generate a base XML file for OpenMalaria. This file is used to generate
##'   the scenarios for simulation.
##' @details The 'data' argument is a nested list resembling the input XML of
##'   OpenMalaria. The attribute names and possible values can be found in
##'   OpenMalaria's schema documentation. Some attributes are not part of the
##'   official schema, like 'expName'. More details can be found in the
##'   vignette.
##' @param data List containing all information
##' @param replace Overwrite output XML if it is already present.
##'   Possible values are TRUE, FALSE, or "ask".
##' @export
createBaseXml <- function(data = NULL, replace = "ask") {
  ## Check if experiment directory is defined
  tryCatch(
    expDir <- getCache(x = "experimentDir"),
    error = function(c) {
      stop(
        "Experiment directory could not be found. Please run 'setupDirs' or 'loadExperiment' first."
      )
    }
  )

  ## Sanity check: simStart needs to be the same as startDate, otherwise the
  ## dates are shifted into oblivion
  if (exists("simStart", envir = .pkgcache)) {
    if (as.Date(data[["monitoring"]][["startDate"]]) != getCache("simStart")) {
      warning(
        paste0(
          "startDate ", data[["monitoring"]][["startDate"]],
          " is not the same as simStart ", getCache("simStart"), ".",
          " This can cause survey times to not appear!"
        )
      )
    }
  }

  ## Variables
  if (is.null(data[["xmlBasename"]])) {
    xmlBasename <- paste0(getCache(x = "experimentName"), "_base")
  } else {
    xmlBasename <- data[["xmlBasename"]]
  }
  putCache(x = "xmlBasename", value = xmlBasename)

  ## Generate document root
  baseXml <- .makeXmlRoot(
    schemaVersion = data[["OMVersion"]],
    name = getCache(x = "experimentName"),
    analysisNo = data[["analysisNo"]]
  )
  putCache(x = "OMVersion", value = data[["OMVersion"]])

  ## Construct xml document
  .xmlMakeDocRec(baseXML = baseXml, data = data)

  ## Cache path of XML file
  putCache(x = "baseXml", value = file.path(
    getCache(x = "experimentDir"),
    paste0(xmlBasename, ".xml")
  ))

  ## Create identifiers for third_dimension table
  ##
  ## NOTE
  ## Currently, we support age groups and cohorts. In case we need to expand to
  ## drugs, genotypes, etc., this should be handled here as well.
  thirdDimension <- .thirdDimensionGen()
  ## Store information in cache
  putCache(x = "thirdDimension", value = thirdDimension)

  ## Write base xml file
  createFile <- NULL
  if (file.exists(getCache(x = "baseXml"))) {
    .printDebug(
      paste0("XML file ", getCache(x = "baseXml"), " already present.")
    )
    ## File present, no replace
    if (replace == FALSE) {
      .printDebug("Replace is FALSE, keeping XML file.")
      stop("XML file already present. Aborting.")
    } else if (replace == "ask") {
      answer <- utils::askYesNo("Directory with experiment name already present. Replace?")
      ## No or no answer
      if (!answer == TRUE | is.na(answer)) {
        .printDebug("Answer was no or not given.")
        stop("Aborting.")
        ## Yes
      } else {
        .printDebug("Answer was yes, removing XML file.")
        createFile <- TRUE
        unlink(getCache(x = "baseXml"))
      }
      ## Directory present, replace
    } else {
      .printDebug("Replace is TRUE, removing XML file.")
      createFile <- TRUE
      unlink(getCache(x = "baseXml"))
    }
    ## File not present
  } else {
    .printDebug(
      paste0("XML file ", getCache(x = "baseXml"), " not found. Creating.")
    )
    createFile <- TRUE
  }

  if (createFile == TRUE) {
    .printDebug(paste0("Writing XML file to ", getCache(x = "baseXml")))
    xml2::write_xml(baseXml, file = getCache(x = "baseXml"))
  }

  ## Write cache
  .synchronizeCache(direction = "none")
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
      ## Check if experiment directory is defined
      tryCatch(
        dir <- getCache("experimentDir"),
        error = function(c) {
          stop(
            "Experiment directory could not be found. Please run 'setupDirs' or 'loadExperiment' first."
          )
        }
      )
    }

    ## Utility files
    for (f in c("autoRegressionParameters.csv", "densities.csv")) {
      .printDebug(paste0("Trying to download ", f))
      if (!file.exists(file.path(dir, f))) {
        # Copy shipped file
        file.copy(
          from = file.path(
            system.file(package = "openMalariaUtilities"), "extdata", f
          ),
          to = file.path(dir, f), overwrite = TRUE
        )
        ## utils::download.file(
        ##   url = paste0(
        ##     "https://raw.githubusercontent.com/SwissTPH/openmalaria/schema-",
        ##     version,
        ##     "/test/", f
        ##   ),
        ##   destfile = file.path(dir, f)
        ## )
      } else {
        message(paste0("File ", f, " already exists, skipping."))
      }
    }

    ## Schema file
    f <- paste0("scenario_", major, ".xsd")
    .printDebug(paste0("Trying to download ", f))
    if (!file.exists(file.path(dir, f))) {
      # Copy shipped file
      file.copy(
        from = file.path(
          system.file(package = "openMalariaUtilities"), "extdata", f
        ),
        to = file.path(dir, f), overwrite = TRUE
      )
      ## utils::download.file(
      ##   url = paste0(
      ##     "https://raw.githubusercontent.com/SwissTPH/openmalaria/schema-",
      ##     version,
      ##     "/schema/", f
      ##   ),
      ##   destfile = file.path(dir, f)
      ## )
    } else {
      message(paste0("File ", f, " already exists, skipping."))
    }
  } else {
    stop(paste0(
      "Only the following major versions of Open Malaria are supported: ",
      suppVers
    ))
  }
}

##' @rdname setupOM
##' @export
setup_om <- setupOM
