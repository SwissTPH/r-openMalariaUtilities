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


## Create a SQLite database in the root directory and return connection
## TODO How do we handle the case if the database is already present?
.createDB <- function(dbName, path = getCache("rootDir")) {
  con <- DBI::dbConnect(
    RSQLite::SQLite(), file.path(path, paste0(dbName, ".sqlite"))
  )
  DBI::dbExecute(conn = con, statement = "PRAGMA foreign_keys = ON;")
  return(con)
}

## Create the table layout
## Experiment table
## Scenarios table (with metadata)
## Placeholder table
## Results table
## Cascade changes on delete and update
.createTables <- function(connection) {
  ## Experiments table
  query <- DBI::dbSendQuery(
    conn = connection,
    statement = "CREATE TABLE IF NOT EXISTS experiments (
experiment_id INTEGER PRIMARY KEY,
name TEXT NOT NULL UNIQUE
);"
  )
  DBI::dbClearResult(query)

  ## Use a composite primary keys to uniquely identify scenarios and experiment

  ## Scenarios table
  query <- DBI::dbSendQuery(
    conn = connection,
    statement = "CREATE TABLE IF NOT EXISTS scenarios (
experiment_id INTEGER NOT NULL,
scenario_id INTEGER NOT NULL,
PRIMARY KEY (experiment_id, scenario_id),
FOREIGN KEY (experiment_id) REFERENCES experiments (experiment_id)
    ON DELETE CASCADE ON UPDATE CASCADE);"
  )
  DBI::dbClearResult(query)

  ## Scenarios' metadata table
  ## PRIMARY KEY (experiment_id, scenario_id, key_var),
  query <- DBI::dbSendQuery(
    conn = connection,
    statement = "CREATE TABLE IF NOT EXISTS scenarios_metadata (
experiment_id INTEGER NOT NULL,
scenario_id INTEGER NOT NULL,
key_var TEXT NOT NULL,
value NOT NULL,
FOREIGN KEY (experiment_id, scenario_id) REFERENCES scenarios (experiment_id, scenario_id)
    ON DELETE CASCADE ON UPDATE CASCADE);"
  )
  DBI::dbClearResult(query)

  ## Scenarios' placeholder table
  ## PRIMARY KEY (experiment_id, scenario_id, placeholder),
  query <- DBI::dbSendQuery(
    conn = connection,
    statement = "CREATE TABLE IF NOT EXISTS placeholders (
experiment_id INTEGER NOT NULL,
scenario_id INTEGER NOT NULL,
placeholder TEXT NOT NULL,
value NOT NULL,
FOREIGN KEY (experiment_id, scenario_id) REFERENCES scenarios (experiment_id, scenario_id)
    ON DELETE CASCADE ON UPDATE CASCADE);"
  )
  DBI::dbClearResult(query)

  ## Results table
  ## Column names based on
  ## https://github.com/SwissTPH/openmalaria/wiki/MonitoringOutput#surveys
  query <- DBI::dbSendQuery(
    conn = connection,
    statement = "CREATE TABLE IF NOT EXISTS results (
experiment_id INTEGER NOT NULL,
scenario_id INTEGER NOT NULL,
survey_date TEXT NOT NULL,
third_dimension NOT NULL,
measure TEXT NOT NULL,
value NUMERIC NOT NULL,
FOREIGN KEY (experiment_id, scenario_id) REFERENCES scenarios (experiment_id, scenario_id)
    ON DELETE CASCADE ON UPDATE CASCADE);"
  )
  DBI::dbClearResult(query)
}

##' @title Dictionary mapping survey measure numbers to names
##' @keywords internal
##' @description See:
##'   https://github.com/SwissTPH/openmalaria/wiki/MonitoringOptions
.numberToSurveyMeasure <- function() {
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
    )
  )
  return(dict)
}

## Read a _out.txt file, apply modifications and return a data frame which
## should be added to the DB
.readOutputFile <- function(f) {
  output <- data.table::fread(f)
  ## Assing column names
  colnames(output) <- c(
    "survey_date", "third_dimension", "measure", "value"
  )
  ## Translate measure indices
  ## Read dictionary
  dict <- .numberToSurveyMeasure()
  ## Add column to join on
  output[, measure_index := measure]
  ## Perform join and drop added column
  output <- output[, measure := dict[output,
    measure_name,
    on = "measure_index"
  ]][, c("measure_index") := NULL]

  ## Translate survey time points into dates
  ## Get cached dates
  surveyTimes <- getCache("surveyTimes")
  ## Add column to join on
  ## The survey_date column in the file corresponds to the row number of the
  ## cached dates (e.g. if last survey_date = 422, then we should have 422
  ## unique cached dates)
  output[, rowNum := survey_date]
  surveyTimes[, rowNum := .I]
  ## Perform join and drop added column
  output <- output[, survey_date := surveyTimes[output,
    date,
    on = "rowNum"
  ]][, c("rowNum") := NULL]
  output <- output[, survey_date := as.character(survey_date)]

  return(output)
}

.addExpToDB <- function(connection, x) {
  tryCatch(
    query <- DBI::dbSendQuery(
      conn = connection,
      ## Use a manual statement to insert the experiment so we can rely on SQL
      ## to assing the experiment_id value.
      statement = paste0("INSERT INTO experiments (name) VALUES(\"", x, "\")")
    ),
    error = function(c) {
      message(paste0("An error occured. Make sure that the experiment name is not used already.\n", c))
    }
  )
  DBI::dbClearResult(query)
}

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

.addResultsToDB <- function(connection, x) {
  ## Add to DB
  DBI::dbWriteTable(
    conn = connection,
    name = "results",
    value = x,
    append = TRUE
  )
}

readResults <- function(expDir, dbName, dbDir = NULL) {
  ## Load the cache from disk
  loadExperiment(path = expDir)

  ## Create connection
  if (is.null(dbDir)) {
    dbDir <- getCache("rootDir")
  }
  dbCon <- .createDB(dbName = dbName, path = dbDir)

  ## Create table schema
  .createTables(connection = dbCon)

  ## Add experiment
  .addExpToDB(connection = dbCon, x = getCache("experimentName"))
  ## Add scenarios and placeholders
  scenarios <- data.table::as.data.table(
    readScenarios(experimentDir = getCache("experimentDir"))
  )
  experiment_id <- data.table::data.table(DBI::dbReadTable(dbCon, "experiments"))[name == getCache("experimentName"), experiment_id]
  scenarios <- data.table::data.table(
    scenarios,
    experiment_id
  )

  placeholders <- getCache("placeholders")
  .addScenToDB(connection = dbCon, x = scenarios[, !..placeholders])

  cols <- c("ID", "experiment_id", placeholders)
  .addPlaceholdersToDB(connection = dbCon, x = scenarios[, ..cols])

  ## Add results
  scenarioFiles <- scenarios[["file"]]
  fileNotFound <- c()
  for (file in scenarioFiles) {
    scenario_id <- scenarios[file == file][["ID"]]
    file <- gsub(pattern = ".xml", replacement = "_out.txt", x = file)
    file <- file.path(getCache("outputsDir"), file)
    if (file.exists(file)) {
      input <- .readOutputFile(file)
      input <- data.table::data.table(
        input,
        experiment_id,
        scenario_id
      )
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
  DBI::dbDisconnect(conn = dbCon)
}
