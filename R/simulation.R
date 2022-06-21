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
runSimulations <- function(scenarios, cmd = "openMalaria", dryRun = FALSE,
                           verbose = FALSE, ncores = 1, rowStart = NULL,
                           rowEnd = NULL) {
  ## Use all rows of given scenarios unless rowStart and rowEnd are both given
  range <- .scenariosRowSelect(
    scenarios = scenarios, rowStart = rowStart, rowEnd = rowEnd
  )

  ## Toggle verbose output of OpenMalaria
  if (verbose == TRUE || get("debugOutput", envir = .pkgenv)) {
    verbose <- " --verbose "
  } else {
    verbose <- NULL
  }

  cmd <- ifelse(dryRun == TRUE, cmd, Sys.which(cmd))
  scenarios <- file.path(
    getCache(x = "scenariosDir"),
    scenarios[range, "file"]
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
    fullCmd <- paste0(
      cmd, " --resource-path ", resources, " --scenario ",
      scenario, " --output ", output, " --ctsout ", ctsout, verbose
    )
    ## Collect required information to run Open Malaria
    cmds[[i]] <- list(
      ## Workind directory
      newWd = getCache(x = "experimentDir"),
      oldWd = getwd(),
      ## Scenarios number
      num = i,
      ## Command to execute
      cmd = fullCmd,
      ## Logfile destinations
      logfile = file.path(
        getCache(x = "logsDir"), "simulation", paste0(basenameScen, ".log")
      ),
      Errlogfile = file.path(
        getCache(x = "logsDir"), "simulation", paste0(basenameScen, "_error.log")
      )
    )
  }

  ## Run scenario via Open Malaria
  runSim <- function(x, l, dryRun) {
    cmd <- x[["cmd"]]
    ## Open new sink connections
    zz <- file(x[["logfile"]], open = "wt")
    zzErr <- file(x[["Errlogfile"]], open = "wt")
    sink(zz, split = TRUE)
    sink(zzErr, type = "message")

    ## REVIEW Temporarily change working directory (Not good style!)
    setwd(x[["newWd"]])
    print(paste0("Running scenario [", x[["num"]], "/", l, "]"))

    ## Execute command
    ## REVIEW I think this should be wrapped in tryCatch in order to recover
    ##        from an error and make sure that the sinks get closed.
    if (dryRun == TRUE) {
      result <- print(cmd)
    } else {
      result <- system(command = cmd, intern = TRUE)
    }

    print(result)
    ## Close sinks
    sink(type = "message")
    sink()
    ## REVIEW Revert change working directory
    setwd(x[["oldWd"]])
  }

  ## Use parallel if ncores > 1
  if (ncores > 1) {
    cl <- parallel::makeCluster(ncores)
    ## invisible(
      parallel::parLapply(
        cl = cl, cmds, runSim, l = length(cmds), dryRun = dryRun
      )
    ## )
    parallel::stopCluster(cl)
  } else {
    invisible(lapply(cmds, runSim, l = length(cmds), dryRun = dryRun))
  }
}

##' @rdname runSimulations
##' @export
run_simulations <- runSimulations
