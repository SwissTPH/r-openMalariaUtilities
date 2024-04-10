### Launch openMalaria and run the scenarios.


## Load order
##' @include cache.R
NULL


##' @title Run openMalaria scenarios
##' @description Generates a system call to launch OpenMalaria with the correct
##'   parameters for each scenario found in the directory. It is assumed that
##'   OpenMalaria is available in the PATH.
##' @param scenarios Scenario data frame
##' @param cmd Command to run openMalaria.
##' @param dryRun If TRUE, only the final command is printed but not executed.
##' @param verbose If TRUE, use OpenMalaria's verbose output.
##' @param ncores Number of parallel processes to use.
##' @param rowStart Starting row. Optional.
##' @param rowEnd End row. Optional.
##' @export
runSimulations <- function(scenarios = NULL, cmd = "openMalaria", dryRun = FALSE,
                           verbose = FALSE, ncores = 1, rowStart = NULL,
                           rowEnd = NULL) {
  ## Input verification
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(scenarios, null.ok = TRUE, add = assertCol)
  checkmate::assertCharacter(cmd, add = assertCol)
  checkmate::assertLogical(dryRun, add = assertCol)
  checkmate::assertLogical(verbose, add = assertCol)
  checkmate::assertNumber(ncores, add = assertCol)
  checkmate::assertNumber(rowStart, null.ok = TRUE, add = assertCol)
  checkmate::assertNumber(rowEnd, null.ok = TRUE, add = assertCol)
  checkmate::reportAssertions(assertCol)

  ## If scenarios is NULL, simply copy the base xml file
  if (is.null(scenarios)) {
    file.copy(
      from = getCache(x = "baseXml"),
      to = file.path(
        getCache(x = "scenariosDir"),
        paste0(getCache(x = "xmlBasename"), ".xml")
      )
    )
  } else {
    ## Use all rows of given scenarios unless rowStart and rowEnd are both given
    range <- .scenariosRowSelect(
      scenarios = scenarios, rowStart = rowStart, rowEnd = rowEnd
    )
  }

  ## Toggle verbose output of OpenMalaria
  if (verbose == TRUE || get("debugOutput", envir = .pkgenv)) {
    verbose <- " --verbose "
  } else {
    verbose <- NULL
  }

  cmd <- ifelse(dryRun == TRUE, cmd, Sys.which(cmd))
  scenarios <- file.path(
    getCache(x = "scenariosDir"),
    if (is.null(scenarios)) {
      paste0(getCache(x = "xmlBasename"), ".xml")
    } else {
      scenarios[range, "file"]
    }
  )

  ## Check if openMalaria is installed and that scenarios exist in directory.
  errors <- FALSE
  msgs <- c()
  if (is.null(cmd) | cmd == "") {
    errors <- TRUE
    msgs <- append("openMalaria could not be found.", msgs, after = 0)
  }
  if (length(scenarios) == 0) {
    errors <- TRUE
    msgs <- append("No scenarios found.", msgs, after = 0)
  }
  if (errors == TRUE) {
    stop(paste(paste(msgs, collapse = "\n"), "Aborting.", sep = "\n"))
  }

  cmds <- list()
  for (i in seq_len(length(scenarios))) {
    resources <- file.path(getCache(x = "experimentDir"))
    scenario <- scenarios[[i]]
    basenameScen <- sub(
      pattern = "(.*)\\..*$", replacement = "\\1", basename(scenario)
    )
    output <- file.path(
      getCache(x = "outputsDir"),
      paste0(basenameScen, "_out.txt")
    )
    ctsout <- file.path(
      getCache(x = "outputsDir"),
      paste0(basenameScen, "_cts.txt")
    )
    ## Print current step
    args <- paste0(
      "--resource-path \"", resources, "\" --scenario \"", scenario, "\" --output \"",
      output, "\" --ctsout \"", ctsout, "\"", verbose
    )
    ## Collect required information to run Open Malaria
    cmds[[i]] <- list(
      ## Workind directory
      newWd = getCache(x = "experimentDir"),
      oldWd = getwd(),
      ## Scenario number
      num = i,
      ## Command to execute
      cmd = cmd,
      args = args,
      ## Logfile destinations
      logfile = file.path(
        getCache(x = "logsDir"), "simulation", paste0(basenameScen, ".log")
      ),
      errlogfile = file.path(
        getCache(x = "logsDir"), "simulation", paste0(basenameScen, "_error.log")
      )
    )
  }

  ## Run scenario via Open Malaria
  runSim <- function(x, l, dryRun) {
    ## This function is supposed to be able to run via R's parallel clusters.
    ## The nodes do not inherit the environment, so we need to make sure to have
    ## the required objects available.
    library(openMalariaUtilities, include.only = "processFile")

    ## Limit data.table to a single thread in order to avoid nested
    ## parallelization.
    data.table::setDTthreads(1)

    ## Wrap whole function into tryCatch so we can make sure that even if
    ## something fails, we close all sinks and revert the working directory.
    tryCatch(
      {
        ## Open new sink connections
        zz <- file(x[["logfile"]], open = "wt")
        zzErr <- file(x[["errlogfile"]], open = "wt")
        ## Redirect both outputs to a designated file. Unfortunately, R is not
        ## able to split 'message', 'warning', etc. level so have to handle that
        ## oursevles.
        sink(zz, split = TRUE)
        sink(zzErr, type = "message")

        ## REVIEW Temporarily change working directory (Not good style!)
        setwd(x[["newWd"]])
        print(paste0("Running scenario [", x[["num"]], "/", l, "]"))

        ## Execute command, store output
        if (dryRun == TRUE) {
          result <- paste0(x[["cmd"]], " ", x[["args"]], "\n")
        } else {
          result <- system2(command = x[["cmd"]], args = x[["args"]], stdout = TRUE, stderr = TRUE)
        }

        ## Store output into temporary file
        tmpLog <- file.path(tempdir(), paste0("sim-log_", x[["num"]], ".txt"))
        cat(result, file = tmpLog)

        ## Read logfile and remove duplicate entries. This reduces individual
        ## log file size and length massively.
        result <- processFile(tmpLog)

        ## Append the logs to designated logfile
        cat(result, file = x[["logfile"]], sep = "\n", append = TRUE)
      },
      finally = {
        ## Close sinks and connections
        sink(type = "message")
        sink()
        close(con = zz)
        close(con = zzErr)

        ## REVIEW Revert change working directory
        setwd(x[["oldWd"]])

        ## Check if logfiles are empty. If yes, remove them to save a bit space.
        for (f in c(x[["logfile"]], x[["errlogfile"]])) {
          ## If file size = 0 bytes, remove
          ## If the file contains only whitespace, also remove
          if (file.size(f) == 0 || length(processFile(f)) == 0) {
            unlink(f)
          }
        }
      }
    )
  }

  ## Use parallel if ncores > 1
  if (ncores > 1) {
    tryCatch(
      {
        cl <- parallel::makeCluster(ncores, outfile = "")
        invisible(
          parallel::parLapply(
            cl = cl, cmds, runSim, l = length(cmds), dryRun = dryRun
          )
        )
      },
      finally = {
        parallel::stopCluster(cl)
      }
    )
  } else {
    invisible(lapply(cmds, runSim, l = length(cmds), dryRun = dryRun))
  }
}

##' @rdname runSimulations
##' @export
run_simulations <- runSimulations
