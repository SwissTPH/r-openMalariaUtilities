### Launch openMalaria and run the scenarios.

##' @title Run openMalaria scenarios
##' @description Generates a system call to launch OpenMalaria with the correct
##'   parameters for each scenario found in the directory. It is assumed that
##'   OpenMalaria is available in the PATH.
##' @param scenariosDir Directory containing the scenario xml files. Defaults to
##'   the cached scenario directory.
##' @param cmd Command to run openMalaria.
##' @param dryRun If TRUE, only the final command is printed but not executed.
##' @param verbose If TRUE, use OpenMalaria's verbose output.
##' @export
runScenarios <- function(scenariosDir = NULL, cmd = "openMalaria",
                         dryRun = FALSE, verbose = FALSE) {
  ## Get values from cache if not given
  if (is.null(scenariosDir)) {
    scenariosDir <- get(x = "scenariosDir", envir = .pkgcache)
  }

  ## Toggle verbose output of OpenMalaria
  if (verbose == TRUE) {
    verbose <- " --verbose "
  } else {
    verbose <- NULL
  }

  cmd <- Sys.which(cmd)
  scenarios <- list.files(
    path = scenariosDir, pattern = "*.xml$", full.names = TRUE
  )

  ## Check if openMalaria is installed and that scenarios exist in directory.
  errors <- FALSE
  msgs <- c()
  if (is.null(cmd) | cmd == "") {
    errors <- TRUE
    msgs <- append("openMalaria could not be found.", msgs, after = 0)
  }
  if (dir.exists(scenariosDir) == FALSE | length(scenarios) == 0) {
    errors <- TRUE
    msgs <- append("Scenarios directory does not exist or is empty.", msgs,
      after = 0
    )
  }
  if (errors == TRUE) {
    stop(paste(paste(msgs, collapse = "\n"), "Aborting.", sep = "\n"))
  }

  for (i in seq_len(length(scenarios))) {
    resources <- file.path(get(x = "experimentDir", envir = .pkgcache))
    scenario <- scenarios[[i]]
    output <- file.path(
      get(x = "outputsDir", envir = .pkgcache),
      paste0(
        sub(
          pattern = "(.*)\\..*$",
          replacement = "\\1",
          basename(scenario)
        ),
        "_out.txt"
      )
    )
    ctsout <- file.path(
      get(x = "outputsDir", envir = .pkgcache),
      paste0(
        sub(
          pattern = "(.*)\\..*$",
          replacement = "\\1",
          basename(scenario)
        ),
        "_cts.txt"
      )
    )
    ## Print current step
    print(paste0("Running scenario [", i, "/", length(scenarios), "]"))
    fullCmd <- paste0(
      cmd, " --resource-path ", resources, " --scenario ",
      scenario, " --output ", output, " --ctsout ", ctsout, verbose
    )
    if (dryRun == TRUE) {
      print(fullCmd)
    } else {
      system(command = fullCmd, wait = TRUE)
    }
  }
}
