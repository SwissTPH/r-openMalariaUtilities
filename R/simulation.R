## Launch openMalaria and run the scenarios.

## TODO Make compatible with SLURM scheduler

##' @title Run openMalaria scenarios
##' @param scenariosDir Directory containing the scenario xml files. Defaults to
##'   the cached scenario directory.
##' @export
runScenarios <- function(scenariosDir = omuCache$scenariosDir) {
  scenarios <- list.files(path = scenariosDir, pattern = ".xml", full.names = TRUE)
  for (scen in scenarios) {
    print(scen)
    cmd <- "openMalaria"
    resources <- file.path(omuCache$baseDir)
    scenario <- scen
    output <- file.path(omuCache$outputsDir,
                        paste0(sub(pattern = "(.*)\\..*$",
                                  replacement = "\\1",
                                  basename(scen)),
                              "_out.txt"))
    ctsout <- file.path(omuCache$outputsDir,
                        paste0(sub(pattern = "(.*)\\..*$",
                                  replacement = "\\1",
                                  basename(scen)),
                              "_cts.txt"))
    system(command = paste0(cmd, " --resource-path ", resources, " --scenario ",
                            scenario, " --output ", output, " --ctsout ", ctsout),
           intern = TRUE)
  }
}
