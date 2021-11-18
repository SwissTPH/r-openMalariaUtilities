### Launch openMalaria and run the scenarios.

## TODO Make compatible with SLURM scheduler

##' @title Run openMalaria scenarios
##' @param scenariosDir Directory containing the scenario xml files. Defaults to
##'   the cached scenario directory.
##' @param cmd Command to run openMalaria.
##' @export
runScenarios <- function(scenariosDir = NULL, cmd = "openMalaria") {
  ## Get values from cache if not given
  if (is.null(scenariosDir)) {
    scenariosDir <- .omupkgcache$scenariosDir
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
    resources <- file.path(.omupkgcache$baseDir)
    scenario <- scenarios[[i]]
    output <- file.path(
      .omupkgcache$outputsDir,
      paste0(
        sub(
          pattern = "(.*)\\..*$",
          replacement = "\\1",
          basename(scen)
        ),
        "_out.txt"
      )
    )
    ctsout <- file.path(
      .omupkgcache$outputsDir,
      paste0(
        sub(
          pattern = "(.*)\\..*$",
          replacement = "\\1",
          basename(scen)
        ),
        "_cts.txt"
      )
    )
    ## Print current step
    print(paste0("Running scenario [", i, "/", length(scenarios), "]"))
    system(
      command = paste0(
        cmd, " --resource-path ", resources, " --scenario ",
        scenario, " --output ", output, " --ctsout ", ctsout
      ),
      intern = TRUE
    )
  }
}
