## Validate created base XML file against the schema provided by OpenMalaria

## We simply run 'xml_validate' from the xml2 library and exclude any error
## which is due to the placeholder syntax.

## TODO It would be much better if we could parse the XSD file and store the
## type of the values. Then each could be validated while the XML file gets
## created.


##' @include pkg_setup.R
NULL


##' @title Validate the XML file
##' @param xmlfile Path to XML file.
##' @param schema Path to XSD schema file.
##' @param scenarios Scenario data frame.
##' @export
validateXML <- function(xmlfile = NULL, schema = NULL, scenarios = NULL) {
  ## Verify input
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(xmlfile, null.ok = TRUE, add = assertCol)
  checkmate::assertCharacter(schema, null.ok = TRUE, add = assertCol)
  checkmate::assertDataFrame(scenarios, null.ok = TRUE, add = assertCol)
  checkmate::reportAssertions(assertCol)

  ## If not given, get file path from cache
  if (is.null(xmlfile)) {
    xmlfile <- getCache(x = "baseXml")
  }

  ## If not defined, compute schema file path from cached OM version
  if (is.null(schema)) {
    schema <- file.path(
      getCache("experimentDir"),
      paste0("scenario_", getCache(x = "OMVersion"), ".xsd")
    )
  }

  ## Check if schema and base XML file exist
  if (!file.exists(xmlfile) || !file.exists(schema)) {
    stop("Schema XSD file or XML file could not be found. Make sure they exist.")
  }

  ## Setup return value. If any of the following tests fails, set it to FALSE
  pass <- TRUE

  ## Extract potential errors (stored as attributes)
  errors <- attr(xml2::xml_validate(
    xml2::read_xml(xmlfile),
    xml2::read_xml(schema)
  ), which = "errors")

  ## Collect expected types
  found <- unique(
    utils::strcapture("@(.*?)@.*'xs:(.*?)'",
      errors,
      proto = list(key = character(), xmltype = character())
    )
  )

  ## Add R's equivalent
  typeDict <- data.frame(
    xmltype = c("double", "string", "boolean", "int"),
    rtype = c("double", "character", "logical", "integer")
  )
  found <- merge(found, typeDict, by = "xmltype")

  ## Remove false positives (e.g. errors which are only caused by the
  ## placeholders). Warn about remaining errors.
  fpositives <- errors[!grepl("@(.*?)@", errors)]
  if (length(fpositives) > 0) {
    pass <- FALSE
    warning(
      cat("Validation against schema failed:\n",
        fpositives, "",
        sep = "\n"
      )
    )
  }

  ## If scenarios are given, validate them
  if (!is.null(scenarios)) {
    for (i in colnames(scenarios)) {
      actual <- typeof(scenarios[, i])
      expected <- found[found[, "key"] == i, "rtype"]
      if (length(expected != 0)) {
        if (actual != expected) {
          pass <- FALSE
          warning(
            paste0(
              i, " should be ", expected, " but was found to be ", actual, "."
            )
          )
        }
      }
    }
  }
  return(pass)
}

##' @rdname validateXML
##' @export
validate_xml <- validateXML
