## Run from the repository root after installing the package:
## Rscript .github/validation/run.R 49 /path/to/openMalaria /path/to/artifacts
## Append --prepare-only to check inputs without an executable (use - as path).
library(openMalariaUtilities)
source(".github/validation/scenario.R")

validateOpenMalaria <- function(version, cmd, outputDir, prepareOnly = FALSE) {
  stopifnot(version %in% c(44L, 49L))
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  outputDir <- normalizePath(outputDir, winslash = "/", mustWork = TRUE)
  clearCache()
  setupDirs(paste0("validation_v", version), rootDir = outputDir, replace = FALSE)
  baseList <- makeValidationScenario(version)
  createBaseXml(baseList, replace = FALSE)
  setupOM(version = version)
  scenarios <- finalizeScenarios(data.frame(seed = 1:2))
  setupScenarios(scenarios = scenarios)
  storeScenarios(scenarios)
  stopifnot(validateXML(scenarios = scenarios))
  schemaPath <- file.path(getCache("experimentDir"), paste0("scenario_", version, ".xsd"))
  schema <- xml2::read_xml(schemaPath)
  inputs <- file.path(getCache("scenariosDir"), scenarios$file)
  for (input in inputs) {
    stopifnot(isTRUE(xml2::xml_validate(xml2::read_xml(input), schema)))
    stopifnot(!any(grepl("@seed@", readLines(input), fixed = TRUE)))
  }

  ## Frozen v44 input was generated with the original builders at b24e1d6.
  ## Compare inputs before execution, then compare raw same-version outputs.
  referenceInputs <- character()
  if (version == 44L) {
    reference <- paste(readLines(".github/validation/reference-v44.xml"), collapse = "\n")
    referenceInputs <- file.path(getCache("experimentDir"), paste0("reference_", 1:2, ".xml"))
    for (i in seq_along(inputs)) {
      xml <- xml2::read_xml(gsub("@seed@", scenarios$seed[i], reference, fixed = TRUE))
      stopifnot(identical(as.character(xml2::read_xml(inputs[i])), as.character(xml)))
      xml2::write_xml(xml, referenceInputs[i])
    }
  }
  if (prepareOnly) {
    message("Prepared and schema-validated v", version, " inputs; execution was skipped.")
    return(invisible(outputDir))
  }

  cmd <- normalizePath(cmd, winslash = "/", mustWork = TRUE)
  runChecked <- function(args, logFile) {
    ## Xerces resolves the relative schemaLocation from the working directory.
    ## Match runSimulations(), which executes from the experiment directory.
    oldWd <- setwd(getCache("experimentDir"))
    on.exit(setwd(oldWd), add = TRUE)
    log <- suppressWarnings(system2(cmd, args, stdout = TRUE, stderr = TRUE))
    writeLines(log, logFile)
    status <- attr(log, "status")
    if (!is.null(status) && status != 0L) {
      stop("OpenMalaria failed; see ", logFile)
    }
    log
  }
  versionLog <- runChecked("--version", file.path(outputDir, "executable-version.txt"))
  pattern <- paste0("schema version:[[:space:]]*", version, "([^0-9]|$)|schema-", version, "[.]")
  if (!any(grepl(pattern, versionLog, ignore.case = TRUE))) {
    stop("The executable did not report schema version ", version)
  }
  for (i in seq_along(inputs)) {
    runChecked(
      c("--resource-path", shQuote(getCache("experimentDir")),
        "--scenario", shQuote(inputs[i]), "--validate-only"),
      file.path(outputDir, paste0("preflight_", i, ".log"))
    )
  }

  ## Nonzero system2 status is a warning in the current wrapper. Fail the
  ## integration check on that warning, and also verify complete outputs.
  withCallingHandlers(
    runSimulations(scenarios = scenarios, cmd = cmd, ncores = 1),
    warning = function(w) stop(conditionMessage(w), call. = FALSE)
  )
  outputs <- file.path(getCache("outputsDir"), sub("[.]xml$", "_out.txt", scenarios$file))
  continuous <- sub("_out[.]txt$", "_cts.txt", outputs)
  stopifnot(all(file.exists(c(outputs, continuous))))
  stopifnot(all(file.info(c(outputs, continuous))$size > 0))
  for (i in seq_along(outputs)) {
    raw <- readOutputFile(outputs[i], translate = FALSE)
    stopifnot(nrow(raw) > 0L, all(is.finite(raw$value)))
    stopifnot(identical(sort(unique(raw$survey_date)), getCache("surveyTimes")$number))
    if (version == 44L) {
      referenceOutput <- file.path(outputDir, paste0("reference_", i, "_out.txt"))
      referenceContinuous <- file.path(outputDir, paste0("reference_", i, "_cts.txt"))
      runChecked(c(
        "--resource-path", shQuote(getCache("experimentDir")),
        "--scenario", shQuote(referenceInputs[i]), "--output", shQuote(referenceOutput),
        "--ctsout", shQuote(referenceContinuous)
      ), file.path(outputDir, paste0("reference_", i, ".log")))
      stopifnot(identical(readLines(outputs[i]), readLines(referenceOutput)))
      stopifnot(identical(readLines(continuous[i]), readLines(referenceContinuous)))
    } else {
      stopifnot(all(c(43L, 80L, 81L, 82L, 1043L, 2043L, 3043L) %in% raw$measure))
    }
  }

  expected <- data.table::rbindlist(lapply(seq_along(outputs), function(i) {
    readOutputFile(outputs[i], scenID = scenarios$ID[i])
  }))
  stopifnot(!anyNA(expected$measure), !anyNA(expected$third_dimension), !anyNA(expected$survey_date))
  stopifnot(all(is.finite(expected$value)))
  expectedLabel <- if (version == 49L) "nNewInfections" else "nNewefections"
  stopifnot(expectedLabel %in% expected$measure)
  if (version == 49L) {
    stopifnot(all(c(
      "nNewInfections_Imported", "nNewInfections_Introduced", "nNewInfections_Indigenous",
      "nCMDTReport", "nSevereWithoutComorbidities", "expectedSevereWithoutComorbidities"
    ) %in% expected$measure))
  }
  data.table::setorderv(expected, names(expected))
  for (strategy in c("serial", "batch")) {
    dbName <- paste0("results_", strategy)
    collectResults(
      expDir = getCache("experimentDir"), dbDir = outputDir, dbName = dbName,
      strategy = strategy, ncores = 1, replace = FALSE
    )
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(outputDir, paste0(dbName, ".sqlite")))
    tryCatch({
      results <- data.table::as.data.table(DBI::dbReadTable(con, "results"))
      stopifnot(identical(names(results), c(
        "experiment_id", "scenario_id", "survey_date", "third_dimension", "measure", "value"
      )))
      actual <- results[, names(expected), with = FALSE]
      data.table::setorderv(actual, names(expected))
      stopifnot(isTRUE(all.equal(actual, expected, check.attributes = FALSE)))
      stopifnot(setequal(results$scenario_id, scenarios$ID))
      stopifnot(!anyNA(results), all(is.finite(results$value)))
      stopifnot(nrow(DBI::dbGetQuery(con, "PRAGMA foreign_key_check")) == 0L)
      tableInfo <- DBI::dbGetQuery(con, "PRAGMA table_info(results)")
      stopifnot(identical(tableInfo$type[tableInfo$name == "measure"], "TEXT"))
    }, finally = DBI::dbDisconnect(con))
  }
  writeLines(c(
    paste0("OpenMalaria schema ", version), "Two seeds completed with non-empty survey and continuous output.",
    "All cached survey dates were present; values were finite.",
    "Serial and batch SQLite collection matched the translated raw outputs.",
    if (version == 44L) "Frozen pre-migration v44 inputs, survey and continuous reference results matched." else
      "New clinical and infection-origin measure IDs were present and translated."
  ), file.path(outputDir, "validation-summary.txt"))
  message("OpenMalaria v", version, " integration checks passed: ", outputDir)
  invisible(outputDir)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 3L) stop("Usage: run.R VERSION EXECUTABLE OUTPUT_DIR [--prepare-only]")
  validateOpenMalaria(as.integer(args[1]), args[2], args[3], "--prepare-only" %in% args)
}
