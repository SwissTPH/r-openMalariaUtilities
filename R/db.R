### Collect and store the Open Malaria's results in a database

## Currently, we support SQLite. But in time, this should be extendable towards
## othe database engines like Postgres, MariaDB, etc.


## Load order
##' @include cache.R
NULL


## Create database in rootDir
## Read all _out.txt files in outputsDir and store them in database
## ???
## Profit!


##' @title Create a database connection
##' @description Create a SQLite database in the root directory if it does not
##'   exist and return the connection.
##' @param dbName Name of the database file without extension.
##' @param path Directory of the database file. Defaults to the root directory.
##' @keywords internal
.createDB <- function(dbName, path = getCache("rootDir")) {
  ## Input verification
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(dbName, add = assertCol)
  checkmate::assertCharacter(path, add = assertCol)
  checkmate::reportAssertions(assertCol)

  con <- DBI::dbConnect(
    RSQLite::SQLite(), file.path(path, paste0(dbName, ".sqlite"))
  )
  ## Allow the use of FOREIGN KEY
  DBI::dbExecute(conn = con, statement = "PRAGMA foreign_keys = ON;")
  ## Use Write-Ahead Logging. This will allow multiple connections to the DB.
  DBI::dbExecute(conn = con, statement = "PRAGMA journal_mode=WAL;")
  return(con)
}

##' @title Create the DB table layout, except the results table
##' @description Creates the database schema: experiments, scenarios,
##'   scenarios_metadata, placeholders. Changes are cascaded from down from
##'   experiments table.
##' @param connection Database connection.
##' @keywords internal
.createTables <- function(connection) {
  ## Experiments table
  DBI::dbExecute(
    conn = connection,
    statement = "CREATE TABLE IF NOT EXISTS experiments (
experiment_id INTEGER PRIMARY KEY,
name TEXT NOT NULL UNIQUE
);"
  )

  ## Scenarios table
  DBI::dbExecute(
    conn = connection,
    statement = "CREATE TABLE IF NOT EXISTS scenarios (
experiment_id INTEGER NOT NULL,
scenario_id INTEGER NOT NULL,
PRIMARY KEY (experiment_id, scenario_id),
FOREIGN KEY (experiment_id) REFERENCES experiments (experiment_id)
    ON DELETE CASCADE ON UPDATE CASCADE);"
  )

  ## Scenarios' metadata table
  DBI::dbExecute(
    conn = connection,
    statement = "CREATE TABLE IF NOT EXISTS scenarios_metadata (
experiment_id INTEGER NOT NULL,
scenario_id INTEGER NOT NULL,
key_var TEXT NOT NULL,
value NOT NULL,
FOREIGN KEY (experiment_id, scenario_id) REFERENCES scenarios (experiment_id, scenario_id)
    ON DELETE CASCADE ON UPDATE CASCADE);"
  )

  ## Scenarios' placeholder table
  DBI::dbExecute(
    conn = connection,
    statement = "CREATE TABLE IF NOT EXISTS placeholders (
experiment_id INTEGER NOT NULL,
scenario_id INTEGER NOT NULL,
placeholder TEXT NOT NULL,
value NOT NULL,
FOREIGN KEY (experiment_id, scenario_id) REFERENCES scenarios (experiment_id, scenario_id)
    ON DELETE CASCADE ON UPDATE CASCADE);"
  )
}


##' @title Create the DB result table
##' @description Creates the database schema for the results.
##' @param connection Database connection.
##' @param tName Name of the results table.
##' @param columns A list containing the column names and types, e.g. list(names
##'   = c(scenario_id, value), types = c("INTEGER", "NUMERIC")). The column
##'   "experiment_id" is always automatically added.
##' @keywords internal
.createResultsTable <- function(connection, tName, columns) {
  ## Results table
  ## Column names based on
  ## https://github.com/SwissTPH/openmalaria/wiki/MonitoringOutput#surveys

  ## Add experiment_id
  columns[["names"]] <- c("experiment_id", columns[["names"]])
  columns[["types"]] <- c("INTEGER", columns[["type"]])
  DBI::dbExecute(
    conn = connection,
    statement = paste0(
      "CREATE TABLE IF NOT EXISTS ", paste0(tName), " (",
      paste0(
        columns[["names"]], " ", columns[["types"]], " NOT NULL",
        collapse = ", "
      ),
      ", ",
      "FOREIGN KEY (experiment_id",
      ifelse("scenario_id" %in% columns[["names"]], ", scenario_id", ""),
      ") REFERENCES scenarios (experiment_id",
      ifelse("scenario_id" %in% columns[["names"]], ", scenario_id", ""),
      ") ON DELETE CASCADE ON UPDATE CASCADE);"
    )
  )
}

##' @title Open Malaria output dictionary
##' @description A dictionary which provides translations for the following
##'   outputs of Open Malaria:
##'
##'   - Survey measure numbers to names
##'
##'   - Whether measures are summed up between survey dates (incident = TRUE) or
##'   represent prevalent characteristics (incident = FALSE)
##'
##'   - An identifier for the 'third dimension' column. This can be 'age_group',
##' 'vector_species', 'drug_id' or NA
##'
##' See: https://github.com/SwissTPH/openmalaria/wiki/MonitoringOptions
##' @export
omOutputDict <- function() {
  dict <- data.table::data.table(
    measure_index = as.integer(c(
      0, 1, 2, 3, 4, 5, 6, 7, 8,
      10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
      20, 21, 22, 23, 24, 25, 26, 27,
      30, 31, 32, 33, 34, 35, 36, 39,
      40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
      50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
      60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
      70, 71, 72, 73, 74, 75, 76, 77, 78, 79
    )),
    measure_name = c(
      ## 0 - 8
      "nHost", "nInfect", "nExpectd", "nPatent", "sumLogPyrogenThres",
      "sumlogDens", "totalInfs", "nTransmit", "totalPatentInf",

      ## 10s
      "sumPyrogenThresh", "nTreatments1", "nTreatments2", "nTreatments3",
      "nUncomp", "nSevere", "nSeq", "nHospitalDeaths", "nIndDeaths",
      "nDirDeaths",

      ## 20s
      "nEPIVaccinations", "allCauseIMR", "nMassVaccinations", "nHospitalRecovs",
      "nHospitalSeqs", "nIPTDoses", "annAvgK", "nNMFever",

      ## 30s
      "innoculationsPerAgeGroup", "Vector_Nv0", "Vector_Nv", "Vector_Ov",
      "Vector_Sv", "inputEIR", "simulatedEIR", "Clinical_RDTs",

      ## 40s
      "Clinical_DrugUsage", "Clinical_FirstDayDeaths",
      "Clinical_HospitalFirstDayDeaths", "nNewefections", "nMassITNs",
      "nEPI_ITNs", "nMassIRS", "nMassVA", "Clinical_Microscopy",
      "Clinical_DrugUsageIV",

      ## 50s
      "nAddedToCohort", "nRemovedFromCohort", "nMDAs", "nNmfDeaths",
      "nAntibioticTreatments", "nMassScreenings", "nMassGVI", "nCtsIRS",
      "nCtsGVI", "nCtsMDA",

      ## 60s
      "nCtsScreenings", "nSubPopRemovalTooOld", "nSubPopRemovalFirstEvent",
      "nLiverStageTreatments", "nTreatDiagnostics", "nMassRecruitOnly",
      "nCtsRecruitOnly", "nTreatDeployments", "sumAge", "nInfectByGenotype",

      ## 70s
      "nPatentByGenotype", "logDensByGenotype", "nHostDrugConcNonZero",
      "sumLogDrugConcNonZero", "expectedDirectDeaths", "expectedHospitalDeaths",
      "expectedIndirectDeaths", "expectedSequelae", "expectedSevere",
      "innoculationsPerVector"
    ),
    incident = c(
      ## 0 - 8
      TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA, NA,

      ## 10s
      FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,

      ## 20s
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE,

      ## 30s
      TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE,

      ## 40s
      NA, NA, NA, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,

      ## 50s
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,

      ## 60s
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE,

      ## 70s
      TRUE, FALSE, TRUE, NA, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE
    ),
    third_dimension = c(
      ## 0 - 8
      "age_group", "age_group", "age_group", "age_group", "age_group",
      "age_group", "age_group", NA, "age_group",

      ## 10s
      "age_group", "age_group", "age_group", "age_group", "age_group",
      "age_group", "age_group", "age_group", "age_group", "age_group",

      ## 20s
      "age_group", NA, "age_group", "age_group", "age_group", "age_group", NA,
      "age_group",

      ## 30s
      "age_group", "vector_species", "vector_species", "vector_species",
      "vector_species", NA, NA, NA,

      ## 40s
      "drug_id", "age_group", "age_group", "age_group", "age_group",
      "age_group", "age_group", "age_group", NA, "drug_id",

      ## 50s
      "age_group", "age_group", "age_group", "age_group", "age_group",
      "age_group", "age_group", "age_group", "age_group", "age_group",

      ## 60s
      "age_group", "age_group", "age_group", "age_group", "age_group",
      "age_group", "age_group", "age_group", "age_group", "age_group",

      ## 70s
      "age_group", "age_group", "age_group", "age_group", "age_group",
      "age_group", "age_group", "age_group", "age_group", NA
    )
  )
  return(dict)
}

##' @title Read Open Malaria output file
##' @description Read a '*_out.txt' file, apply modifications and return a data
##'   frame which should be added to the DB.
##' @param f File name to read from.
##' @param filter A data.table expression to filter rows.
##' @param translate Can be any of c("dates", "measures") or simply TRUE
##'   (implies all) or FALSE (implies none).
##' @param scenID Scenario ID to be added.
##' @importFrom data.table ':='
##' @export
readOutputFile <- function(f, filter = NULL, translate = TRUE, scenID = NULL) {
  ## Input verification
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(f, add = assertCol)
  checkmate::assert(
    checkmate::checkSubset(translate, choices = c("measures", "dates")),
    checkmate::checkLogical(translate),
    add = assertCol
  )
  checkmate::reportAssertions(assertCol)

  ## Appease NSE notes in R CMD check
  measure_index <- measure <- measure_name <- number <- rowNum <- NULL
  survey_date <- third_dimension <- NULL

  ## Which columns should be translated?
  ## TRUE means all, FALSE none, otherwise specified
  if (translate == TRUE) {
    translate <- c("dates", "measures")
  } else if (translate == FALSE) {
    translate <- c()
  } else {
    translate <- translate
  }

  output <- data.table::fread(f)
  ## Assing column names
  colnames(output) <- c(
    "survey_date", "third_dimension", "measure", "value"
  )
  ## Translate measure indices
  if ("measures" %in% translate) {
    ## Read dictionary
    dict <- omOutputDict()
    ## Add column to join on
    output[, measure_index := measure]
    ## Perform join and drop added column
    output <- output[, measure := dict[output,
      measure_name,
      on = "measure_index"
    ]][, c("measure_index") := NULL]
  }
  ## Translate survey time points into dates
  if ("dates" %in% translate) {
    ## Get cached dates
    surveyTimes <- getCache("surveyTimes")
    ## Add column to join on
    ## The survey_date column in the file corresponds to the row number of the
    ## cached dates (e.g. if last survey_date = 422, then we should have 422
    ## unique cached dates)
    output[, rowNum := survey_date]
    surveyTimes[, rowNum := number]
    ## Perform join and drop added column
    output <- output[, survey_date := surveyTimes[output,
      date,
      on = "rowNum"
    ]][, c("rowNum") := NULL]
    ## Assign column types
    output <- output[, survey_date := as.character(survey_date)]
  }
  output <- output[, third_dimension := as.character(third_dimension)]

  ## Apply row filter, if specified
  if (!is.null(filter)) {
    filter <- substitute(filter)
    ## HACK This feels like a hack but in case the filter argument is quoted
    ##      (which it is for example when used in parallel's parLapply) it needs
    ##      to be unquoted unitl is a single expression.
    ##      The first argument of an expression is either '&' (which is what we
    ##      want here) or the name of the call (here: quote) which we do not
    ##      want.
    while (filter[[1]] == "quote" | filter[[1]] == "base::quote") {
      filter <- eval(filter)
    }
    output <- output[eval(filter)]
  }
  if (!is.null(scenID)) {
    output <- output[, scenario_id := rep(scenID, times = nrow(output))]
  }

  return(output)
}

##' @title Add data to experiments table in DB
##' @param connection Database connection.
##' @param x Data to add.
##' @keywords internal
.addExpToDB <- function(connection, x) {
  ## Check if the experiment name is already present in the DB
  entries <- DBI::dbReadTable(conn = connection, name = "experiments")[, "name"]
  ## If yes, inform the user and do nothing
  if (x %in% entries) {
    message(paste0(
      "An experiment with the name: ", x, " is already present. ",
      "Data will be appended if possible."
    ))
  } else {
    query <- DBI::dbSendQuery(
      conn = connection,
      ## Use a manual statement to insert the experiment so we can rely on SQL
      ## to assing the experiment_id value.
      statement = paste0("INSERT INTO experiments (name) VALUES(\"", x, "\")")
    )
    DBI::dbClearResult(query)
    return(TRUE)
  }
}

##' @title Add data to scenarios and scenarios_metadata table in DB
##' @param connection Database connection.
##' @param x Data to add.
##' @keywords internal
.addScenToDB <- function(connection, x) {
  ## Create data frame to add
  x <- data.table::as.data.table(x)
  data.table::setnames(x, "ID", "scenario_id")
  ## Add to DB
  DBI::dbWriteTable(
    conn = connection,
    name = "scenarios",
    value = x[, c("experiment_id", "scenario_id")],
    append = TRUE
  )
  ## Add metadata to DB
  x <- data.table::melt(
    x,
    id.vars = c("experiment_id", "scenario_id"),
    variable.name = "key_var", value.name = "value"
  )
  DBI::dbWriteTable(
    conn = connection,
    name = "scenarios_metadata",
    value = x,
    append = TRUE
  )
}

##' @title Add data to placeholders table in DB
##' @param connection Database connection.
##' @param x Data to add.
##' @keywords internal
.addPlaceholdersToDB <- function(connection, x) {
  ## Create data frame to add
  x <- data.table::as.data.table(x)
  data.table::setnames(x, "ID", "scenario_id")
  x <- data.table::melt(
    x,
    id.vars = c("experiment_id", "scenario_id"),
    variable.name = "placeholder", value.name = "value"
  )

  ## Add to DB
  DBI::dbWriteTable(
    conn = connection,
    name = "placeholders",
    value = x,
    append = TRUE
  )
}

##' @title Add data to results table in DB
##' @param connection Database connection.
##' @param x Data to add.
##' @keywords internal
.addResultsToDB <- function(connection, x) {
  ## Add to DB
  DBI::dbWriteTable(
    conn = connection,
    name = "results",
    value = x,
    append = TRUE
  )
}

##' @title Collect Open Malaria results into a database
##' @param expDir Database connection.
##' @param dbName Name of the database file without extension.
##' @param dbDir Directory of the database file. Defaults to the root directory.
##' @param replace How to handle duplicate experiments in the database. If TRUE,
##'   any experiment with the same name will be replaced. If FALSE, a new entry
##'   with the same name will be ignored.
##' @importFrom data.table ':='
##' @export
readResults <- function(expDir, dbName, dbDir = NULL, replace = FALSE) {
  ## Input verification
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(expDir, add = assertCol)
  checkmate::assertCharacter(dbName, add = assertCol)
  checkmate::assertCharacter(dbDir, null.ok = TRUE, add = assertCol)
  checkmate::assertLogical(replace, add = assertCol)
  checkmate::reportAssertions(assertCol)

  ## Appease NSE notes in R CMD check
  name <- survey_date <- NULL

  ## Make sure the cache is up-to-date
  syncCache(path = expDir)

  ## Create connection
  if (is.null(dbDir)) {
    dbDir <- getCache("rootDir")
  }
  dbCon <- .createDB(dbName = dbName, path = dbDir)

  ## Create table schema
  .createTables(connection = dbCon)

  ## Translate replace value
  methodsDict <- data.frame(
    replace = c(TRUE, FALSE),
    method = c("replace", "ignore")
  )
  replace <- methodsDict[methodsDict[, "replace"] == replace, "method"]

  ## Add experiment
  expName <- getCache("experimentName")
  continue <- .addExpToDB(connection = dbCon, x = expName, method = replace)
  if (continue == TRUE) {
    ## Add scenarios and placeholders
    scenarios <- data.table::as.data.table(
      readScenarios(experimentDir = getCache("experimentDir"))
    )
    experiment_id <- data.table::data.table(
      DBI::dbReadTable(
        dbCon, "experiments"
      )
    )[name == expName, experiment_id]
    scenarios <- data.table::data.table(
      scenarios,
      experiment_id = rep(experiment_id, times = nrow(scenarios))
    )

    ## Placeholders can be empty in case there are none. We need to handle that
    ## situation.
    placeholders <- tryCatch(
      getCache("placeholders"),
      error = function(c) {
        .printVerbose("No placeholders found in cache!")
        character(0)
      }
    )
    if (length(placeholders) == 0) {
      .addScenToDB(
        connection = dbCon, x = scenarios
      )
    } else {
      .addScenToDB(
        connection = dbCon, x = scenarios[, !placeholders, with = FALSE]
      )

      cols <- c("ID", "experiment_id", placeholders)
      .addPlaceholdersToDB(
        connection = dbCon, x = scenarios[, cols, with = FALSE]
      )
    }

    ## Add results
    scenarioFiles <- scenarios[["file"]]
    fileNotFound <- c()
    for (f in scenarioFiles) {
      scenario_id <- scenarios[file == f][["ID"]]
      file <- gsub(pattern = ".xml", replacement = "_out.txt", x = f)
      file <- file.path(getCache("outputsDir"), file)
      if (file.exists(file)) {
        input <- readOutputFile(file)
        input <- input[, experiment_id := rep(
          experiment_id,
          times = nrow(input)
        )]
        input <- input[, scenario_id := rep(scenario_id, times = nrow(input))]
        ## Change date to unix timestamp. This leads to a nice perfomance
        ## increase if the dates are queried.
        input <- input[, survey_date := as.numeric(
          as.POSIXct(survey_date, tz = "UTC")
        )]

        DBI::dbWriteTable(
          conn = dbCon,
          name = "results",
          value = input,
          append = TRUE
        )
      } else {
        fileNotFound <- c(fileNotFound, file)
      }
    }
    if (length(fileNotFound) != 0) {
      warning(paste0("The following results could not be found:\n"), fileNotFound)
    }
    ## Generate the indices of the results table. Indexing provides a
    ## performance boost at the cost of larger size. We add them once at the
    ## end.
    DBI::dbSendQuery(
      conn = dbCon, "CREATE INDEX scen_index ON results(scenario_id);"
    )
  }
  DBI::dbDisconnect(conn = dbCon)
}
