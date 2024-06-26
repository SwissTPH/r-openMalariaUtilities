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
  colNames <- c("experiment_id", columns[["names"]])
  colTypes <- c("INTEGER", columns[["types"]])
  DBI::dbExecute(
    conn = connection,
    statement = paste0(
      "CREATE TABLE IF NOT EXISTS ", paste0(tName), " (",
      paste0(
        colNames, " ", colTypes,
        collapse = ", "
      ),
      ", ",
      "FOREIGN KEY (experiment_id",
      ifelse("scenario_id" %in% colNames, ", scenario_id) REFERENCES scenarios (experiment_id, scenario_id",
        ") REFERENCES experiments (experiment_id"
      ),
      ") ON DELETE CASCADE ON UPDATE CASCADE);"
    )
  )
}

##' @title Read Open Malaria output file
##' @description Read a '*_out.txt' file, apply modifications and return a data
##'   frame which should be added to the DB.
##' @param f File name to read from.
##' @param filter A data.table expression to filter rows.
##' @param translate Can be any of c("dates", "measures", "third_dimension") or
##'   simply TRUE (implies all) or FALSE (implies none).
##' @param scenID Scenario ID to be added.
##' @importFrom data.table ':='
##' @export
readOutputFile <- function(f, filter = NULL, translate = TRUE, scenID = NULL) {
  ## Input verification
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(f, add = assertCol)
  checkmate::assert(
    checkmate::checkSubset(translate,
      choices = c("measures", "dates", "third_dimension")
    ),
    checkmate::checkLogical(translate),
    add = assertCol
  )
  checkmate::reportAssertions(assertCol)

  ## Appease NSE notes in R CMD check
  measure_index <- measure <- measure_name <- number <- rowNum <- NULL
  survey_date <- third_dimension <- scenario_id <- id <- NULL

  ## Which columns should be translated?
  ## TRUE means all, FALSE none, otherwise specified
  if (is.logical(translate)) {
    if (translate == TRUE) {
      translate <- c("dates", "measures", "third_dimension")
    } else {
      translate <- c()
    }
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
  ## Translate third_dimension numerical value to string
  if ("third_dimension" %in% translate) {
    ## Get table from cache
    dict <- getCache("thirdDimension")
    ## Add column to join on
    output[, number := third_dimension]
    ## Perform join and drop added column
    output <- output[, third_dimension := dict[output,
      id,
      on = "number"
    ]][, c("number") := NULL]
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
    ## Make sure that the expression is substituted and unquoted. This can
    ## happen if the function is called from 'do.call(..., quote = TRUE)' or
    ## 'parallel::parLapply()'.
    filter <- substitute(filter)
    filter <- .unqouteExpr(filter)
    output <- output[eval(filter)]
  }
  if (!is.null(scenID)) {
    output <- output[, scenario_id := rep(scenID, times = nrow(output))]
  }

  return(output)
}

##' @title Unqoute expression
##' @param expr Expression
##' @keywords internal
.unqouteExpr <- function(expr) {
  while (is.call(expr) &&
    (expr[[1]] == "quote" || expr[[1]] == "base::quote" ||
      expr[[1]] == "bquote" || expr[[1]] == "base::bquote")) {
    expr <- eval(expr)
  }
  return(expr)
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

##' @title Override default function arguments
##' @param f Function
##' @param args List of function arguments
##' @keywords internal
.setFunArgs <- function(f, args) {
  fails <- c()
  for (n in names(args)) {
    if (n %in% names(formals(f))) {
      formals(f)[[n]] <- args[[n]]
    } else {
      fails <- c(fails, n)
    }
  }
  if (length(fails) > 0) {
    stop(paste(
      paste0(
        "The following arguments are not used in ",
        as.list(match.call())[[2]], ":"
      ),
      paste0(fails, collapse = "\n"),
      sep = "\n"
    ))
  }
  return(f)
}

## REVIEW collectResults is really long and really nested. This should be
##        addressed as soon as the code is stabilized.

##' @title Collect Open Malaria results into a database
##' @param expDir Database connection.
##' @param dbName Name of the database file without extension.
##' @param dbDir Directory of the database file. Defaults to the root directory.
##' @param replace If TRUE, replace an exisiting database with same name as in
##'   dbName. Else, try to append the date to the exisiting database.
##' @param resultsName Name of the database table to add the results to.
##' @param resultsCols A list containing the column names and the column types
##'   of the results table. For example, list(names = c("scenario_id", ...),
##'   types = c("INTEGER", ...)). The "experiment_id" is added automatically.
##'   Types as available for SQLite.
##' @param indexOn Define which index to create. Needs to be a lis of the form
##'   list(c(TABLE, COLUMN), c(TABLE, COLUMN), ...).
##' @param ncores Number of CPU cores to use.
##' @param ncoresDT Number of data.table threads to use on each parallel
##'   cluster.
##' @param strategy Defines how to process the files. "batch" means that all
##'   files are read into a single data frame first, then the aggregation
##'   funciton is applied to that data frame and the result is added to the
##'   database. "serial" means that each individual file is processed with the
##'   aggregation function and added to the database.
##' @param appendResults If TRUE, do not add metadata to the database and only
##'   write results.
##' @param fileFun A function for filtering the input files. Needs to return a
##'   vector of the scenario XML files without path as in the file column of the
##'   scenario data frame. No default.
##' @param fileFunArgs Arguments for fileFun as a (named) list.
##' @param readFun A function for reading and processing OpenMalaria output
##'   files. Needs to return as data frame. The first argument needs to be the
##'   file name and it needs to have ... as an argument. Scenario IDs are
##'   available by using scenID as an argument. If NULL, defaults to
##'   readOutputFile and the scenario IDs are added automatically.
##' @param readFunArgs Arguments for readFun as a (named) list.
##' @param aggrFun A function for aggregating the output of readFun. First
##'   argument needs to be the output data frame of readFun and it needs to
##'   generate a data frame. The data frame should NOT contain an experiment_id
##'   column as this is added automatically. The column names needs to match the
##'   ones defined in resultsCols.
##' @param aggrFunArgs Arguments for aggrFun as a (named) list.
##' @param verbose Boolean, toggle verbose output.
##' @importFrom data.table ':='
##' @export
collectResults <- function(expDir, dbName, dbDir = NULL, replace = FALSE,
                           resultsName = "results", resultsCols = list(
                             names = c(
                               "scenario_id", "survey_date",
                               "third_dimension", "measure", "value"
                             ),
                             types = c("INTEGER", "TEXT", "", "TEXT", "NUMERIC")
                           ),
                           indexOn = list(c("results", "scenario_id")),
                           ncores = 1, ncoresDT = 1, strategy = "serial",
                           appendResults = FALSE,
                           fileFun = NULL, fileFunArgs = NULL,
                           readFun = NULL, readFunArgs = NULL,
                           aggrFun = NULL, aggrFunArgs = NULL,
                           verbose = get("verboseOutput", envir = .pkgenv)) {
  ## Input verification
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertSubset(
    strategy,
    choices = c("batch", "serial"), add = assertCol
  )
  checkmate::assertIntegerish(ncores, add = assertCol)
  checkmate::assertIntegerish(ncoresDT, add = assertCol)
  checkmate::reportAssertions(assertCol)

  ## Number of data.table threads cannot be larger than number of CPU cores
  if (ncoresDT > ncores) {
    stop("ncoresDT cannot be larger than ncores!")
  }

  ## Appease NSE notes in R CMD check
  name <- NULL

  ## Get path if not given
  if (is.null(dbDir)) {
    dbDir <- getCache("rootDir")
  }

  ## Remove database if replace is TRUE
  if (replace) {
    unlink(file.path(dbDir, paste0(dbName, ".sqlite")))
    unlink(file.path(dbDir, paste0(dbName, ".sqlite-shm")))
    unlink(file.path(dbDir, paste0(dbName, ".sqlite-wal")))
  }

  ## Store current DT threads setting
  curDTthreads <- data.table::getDTthreads()

  ## Create connection
  dbCon <- .createDB(dbName = dbName, path = dbDir)

  ## Whatever happens now, we need to make sure that we close the connection.
  ## Because we use WAL, it can happen that if a failure occurs and we do not
  ## close the connection, the next transactions cause the wal file to grow
  ## indefinitely. wal2 for SQLite solves that but is not released at the time
  ## of writing.

  ## Create table schema
  tryCatch(
    {
      .printVerbose("Creating database tables", toggle = verbose)
      .createTables(connection = dbCon)
      .createResultsTable(
        connection = dbCon, tName = resultsName, columns = resultsCols
      )

      ## Drop index if already present. This will speed up the addition of data
      ## but we need to rebuild it.
      if (!is.null(indexOn)) {
        .printVerbose("Dropping old database index", toggle = verbose)
        for (i in seq_along((indexOn))) {
          DBI::dbExecute(
            conn = dbCon,
            statement = paste0(
              "DROP INDEX IF EXISTS ",
              paste0(indexOn[[i]][1], "_", indexOn[[i]][2], "_index"), ";"
            )
          )
        }
      }

      ## Add experiment
      expName <- getCache("experimentName")
      .printVerbose("Adding experiment ID to database", toggle = verbose)
      .addExpToDB(connection = dbCon, x = expName)

      ## By default, select all files from scenarios data frame
      scenarios <- data.table::as.data.table(
        readScenarios(experimentDir = getCache("experimentDir"))
      )

      experiment_id <- data.table::data.table(
        DBI::dbReadTable(
          dbCon, "experiments"
        )
      )[name == expName, experiment_id]

      ## Prepare functions
      ## If readFun is NULL, use default
      if (is.null(readFun)) {
        readFun <- readOutputFile
      }

      ## Set the provided arguments as defaults. This makes it
      fileFun <- .setFunArgs(f = fileFun, args = fileFunArgs)
      readFun <- .setFunArgs(f = readFun, args = readFunArgs)
      aggrFun <- .setFunArgs(f = aggrFun, args = aggrFunArgs)

      if (is.null(fileFun)) {
        files <- scenarios[, file]
      } else {
        files <- do.call(what = fileFun, args = list())
      }

      ## Limit scenarios to selected files and add experiment_id column
      scenarios <- data.table::data.table(
        scenarios[file %in% files],
        experiment_id = rep(
          experiment_id,
          times = nrow(scenarios[file %in% files])
        )
      )

      ## Transform file names to match the content of the output directory
      files <- file.path(
        getCache("outputsDir"),
        gsub(pattern = ".xml", replacement = "_out.txt", x = files)
      )

      ## Check that requested files exist
      fNoExist <- files[!sapply(files, file.exists, USE.NAMES = FALSE)]
      if (length(fNoExist) > 0) {
        ## If fNoExist and files are the same, abort
        if (length(fNoExist) == length(files)) {
          stop("No output files found!")
        }
        ## Otherwise report the missing files
        warning(
          paste0("The following files were not found and thus, will not be processed:\n",
            paste0(fNoExist, collapse = "\n"),
            sep = "\n"
          )
        )

        ## Remove the missing files from scenarios
        scenarios <- scenarios[sapply(files, file.exists, USE.NAMES = FALSE)]
        files <- files[sapply(files, file.exists, USE.NAMES = FALSE)]
      }

      ## Skip addition of metadata if appendResults is TRUE
      if (!appendResults) {
        ## Placeholders can be empty in case there are none. We need to handle
        ## that situation.
        placeholders <- tryCatch(
          getCache("placeholders"),
          error = function(c) {
            .printVerbose("No placeholders found in cache!", toggle = verbose)
            character(0)
          }
        )

        .printVerbose("Adding metadata to database", toggle = verbose)
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
      }

      ## Two strategies to process the data and add them to the DB:
      ##
      ## "batch": Read all files into one data.table, apply aggregation function
      ##          and add the whole output batch to the DB.
      ##
      ## "serial": Read each file individually, apply aggregation function and
      ##           add the output to the DB.
      ##
      ## The reasoning is that a single transaction to the DB should be faster
      ## but aggregating a lot of files can consume a lot of memories. Thus,
      ## depending on the available amount of RAM, it might not be possible to
      ## use that strategy.

      ## Divide assigned computational ressources so data.table threads
      ## and R cluster nodes are balanced and don't compete for
      ## ressources.
      cl_cores <- floor(ncores / ncoresDT)

      if (strategy == "batch") {
        .printVerbose(
          paste0(
            "Using batch strategy with ", ncores,
            " cores and ", ncoresDT, " data.table threads."
          ),
          toggle = verbose
        )

        if (ncores > 1) {
          tryCatch(
            {
              .printVerbose(paste0(
                "Starting R cluster with ", cl_cores,
                " nodes, each using ", ncoresDT, " data.table threads."
              ), toggle = verbose)
              cl <- parallel::makeCluster(cl_cores, outfile = "")
              parallel::clusterExport(cl, "ncoresDT", envir = environment())
              parallel::clusterEvalQ(cl, {
                data.table::setDTthreads(ncoresDT)
                library(openMalariaUtilities)
              })
              parallel::clusterCall(
                cl, "loadExperiment", expDir
              )
              .printVerbose("Reading OM output files", toggle = verbose)
              output <- data.table::rbindlist(
                parallel::clusterMap(
                  cl = cl, readFun, files, scenID = scenarios[["ID"]],
                  SIMPLIFY = FALSE
                )
              )
              .printVerbose("Finished reading OM output files",
                toggle = verbose
              )
            },
            finally = {
              .printVerbose("Stopping R cluster", toggle = verbose)
              parallel::stopCluster(cl)
              data.table::setDTthreads(curDTthreads)
            }
          )
        } else {
          .printVerbose("Reading OM output files", toggle = verbose)
          output <- data.table::rbindlist(
            mapply(readFun, files, scenID = scenarios[["ID"]], SIMPLIFY = FALSE)
          )
          .printVerbose("Finished reading OM output files",
            toggle = verbose
          )
        }

        ## Run aggregation. I don't see a good way to parallelize, so if this is
        ## done, data.table is your friend.
        tryCatch(
          {
            if (ncores > 1) {
              .printVerbose(paste0(
                "Assigning ", ncores, " data.table threads."
              ), toggle = verbose)
              data.table::setDTthreads(ncores)
            }
            if (!is.null(aggrFun)) {
              .printVerbose("Aggregating OM output", toggle = verbose)
              output <- do.call(what = aggrFun, args = list(output))
              .printVerbose("Finished aggregating OM output", toggle = verbose)
            }
          },
          finally = {
            data.table::setDTthreads(curDTthreads)
          }
        )
        ## Add output to DB
        .printVerbose("Adding output to database", toggle = verbose)
        output <- output[, experiment_id := rep(
          experiment_id,
          times = nrow(output)
        )]

        DBI::dbWriteTable(
          conn = dbCon,
          name = resultsName,
          value = output,
          append = TRUE
        )
      } else if (strategy == "serial") {
        .printVerbose(
          paste0(
            "Using serial strategy with ", ncores,
            " cores and ", ncoresDT, " data.table threads."
          ),
          toggle = verbose
        )

        f <- function(file, readFun, aggrFun, db, ...) {
          args <- list(...)
          .printVerbose("Reading OM output files", toggle = verbose)
          output <- do.call(readFun, list(file, scenID = args[["scenID"]]))
          .printVerbose("Finished reading OM output files",
            toggle = verbose
          )
          if (!is.null(aggrFun)) {
            .printVerbose("Aggregating OM output", toggle = verbose)
            output <- do.call(what = aggrFun, args = list(output))
            .printVerbose("Finished aggregating OM output",
              toggle = verbose
            )
          }

          ## Add output to DB
          .printVerbose("Adding output to database", toggle = verbose)
          dbCon <- DBI::dbConnect(RSQLite::SQLite(), db)
          tryCatch(
            {
              output <- output[, experiment_id := rep(
                experiment_id,
                times = nrow(output)
              )]

              DBI::dbWriteTable(
                conn = dbCon,
                name = resultsName,
                value = output,
                append = TRUE
              )
            },
            finally = DBI::dbDisconnect(dbCon)
          )
        }
        if (ncores > 1) {
          tryCatch(
            {
              .printVerbose(paste0(
                "Starting R cluster with ", cl_cores,
                " nodes, each using ", ncoresDT, " data.table threads."
              ), toggle = verbose)
              cl <- parallel::makeCluster(cl_cores, outfile = "")
              parallel::clusterExport(cl, "ncoresDT", envir = environment())
              parallel::clusterEvalQ(cl, {
                data.table::setDTthreads(ncoresDT)
                library(openMalariaUtilities)
              })
              parallel::clusterCall(
                cl, "loadExperiment", expDir
              )
              parallel::clusterMap(
                cl = cl, f, files, scenID = scenarios[["ID"]],
                MoreArgs = list(
                  readFun = readFun, aggrFun = aggrFun,
                  db = file.path(dbDir, paste0(dbName, ".sqlite"))
                ),
                SIMPLIFY = FALSE
              )
            },
            finally = {
              .printVerbose("Stopping R cluster", toggle = verbose)
              parallel::stopCluster(cl)
              data.table::setDTthreads(curDTthreads)
            }
          )
        } else {
          for (fi in files) {
            do.call(f, list(fi,
              scenID = scenarios[file == gsub(
                pattern = "_out.txt", replacement = ".xml", x = basename(fi)
              )][["ID"]],
              readFun = readFun, aggrFun = aggrFun,
              db = file.path(dbDir, paste0(dbName, ".sqlite"))
            ))
          }
        }
      }

      ## Cleanup and optimization
      ## Indexing
      if (!is.null(indexOn)) {
        .printVerbose("Creating database index", toggle = verbose)
        for (i in seq_along((indexOn))) {
          DBI::dbExecute(
            conn = dbCon,
            statement = paste0(
              "CREATE INDEX ",
              paste0(indexOn[[i]][1], "_", indexOn[[i]][2], "_index"),
              " ON ", paste0(indexOn[[i]][1], "(", indexOn[[i]][2], ");")
            )
          )
        }
      }

      .printVerbose("Running database optimizations", toggle = verbose)
      ## Vacuum
      DBI::dbExecute(
        conn = dbCon,
        statement = "VACUUM"
      )
      ## Analyze
      DBI::dbExecute(
        conn = dbCon,
        statement = "ANALYZE"
      )
    },

    ## Terminate connection
    finally = {
      .printVerbose("Done", toggle = verbose)
      DBI::dbDisconnect(conn = dbCon)
    }
  )
}

##' @rdname collectResults
##' @export
collect_results <- collectResults

## DEPRECATED
##' @title Collect Open Malaria results into a database
##' @param expDir Database connection.
##' @param dbName Name of the database file without extension.
##' @param dbDir Directory of the database file. Defaults to the root directory.
##' @param replace How to handle duplicate experiments in the database. If TRUE,
##'   any experiment with the same name will be replaced. If FALSE, a new entry
##'   with the same name will be ignored. DEPRECATED Database will always be
##'   replaced.
##' @importFrom data.table ':='
##' @export
readResults <- function(expDir, dbName, dbDir = NULL, replace = FALSE) {
  warning("readResults has been deprecated. Use collectResults instead.")
  collectResults(
    expDir = expDir, dbName = dbName, dbDir = dbDir, replace = TRUE
  )
}
