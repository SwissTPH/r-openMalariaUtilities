## Launch openMalaria and running the scenarios

runScenarios <- function(scenariosDir = omuCache$scenariosDir) {
  ## Get scenarios in directory
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

## omuCache$scenariosDir <- "~/Projekte/openMalariaUtilitiesTesting/Example_Scenario/example_test/scenarios"
## omuCache$outputsDir <- "~/Projekte/openMalariaUtilitiesTesting/Example_Scenario/example_test/output"

## runScenarios()
