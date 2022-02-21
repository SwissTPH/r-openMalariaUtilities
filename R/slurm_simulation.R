## Run OpenMalaria simulations via SLURM
## The general workflow is:
##  - Get the number of scenarios
##  - Create SLURM launch script and RScript submitting each scenarios to SLURM


## TODO Expose the pre, cmd and post options

##' @title Run preparations for SLURM submission
##' @param expName Name of experiment
##' @param scenarios Scenario data frame
##' @param ntasks Number of tasks per CPU
##' @param memCPU Memory per CPU
##' @param time Maximum time
##' @param qos Quality of service
##' @export
slurmPrepareRunScenarios <- function(expName, scenarios = NULL, ntasks = 1,
                                     memCPU = "250MB", time = "06:00:00",
                                     qos = "6hours") {
  ## Appease NSE notes in R CMD check
  scens <- NULL

  ## Load scenarios from disk if not specified. We did save them in the step
  ## before.
  if (is.null(scenarios)) {
    load(file.path(get(x = "cacheDir", envir = .pkgcache), "scens.RData"))
    scenarios <- scens
  }

  ## Create a submission script
  filename <- file.path(
    get(x = "experimentDir", envir = .pkgcache), "slurm_simulation.sh"
  )
  .writeSlurm(
    jobName = paste0(expName, "_simulation"),
    ntasks = ntasks,
    array = nrow(scenarios),
    time = time,
    qos = qos,
    output = file.path(
      get(x = "logsDir", envir = .pkgcache),
      paste0(expName, "_simulation")
    ),
    error = file.path(
      get(x = "logsDir", envir = .pkgcache),
      paste0(expName, "_simulation")
    ),
    pre = list(
      ## REVIEW It seems that the OpenMalaria and R modules are conflicting.
      ##        Maybe we can find a way around this without forcing autoswap.
      "export LMOD_DISABLE_SAME_NAME_AUTOSWAP=\"no\"",
      "module purge",
      "module load R/4.1.2-foss-2018b-Python-3.6.6",
      "module load OpenMalaria/43.0-iomkl-2019.01",
      ## This is quiet important, otherwise OpenMalaria cannot find the
      ## supporting files (*.xsd, etc)
      paste0("cd ", get(x = "experimentDir", envir = .pkgcache))
    ),
    cmd = list(paste("Rscript", file.path(
      get(x = "experimentDir", envir = .pkgcache), "slurm_run_simulation.R"
    ), "$ID")),
    file = filename
  )

  ## Create R script
  cat(
    "#!/usr/bin/env Rscript

## Get arguments
args <- commandArgs(trailingOnly = TRUE)

## Set correct working directory\n",
    "setwd(dir = \"", paste0(get(x = "experimentDir", envir = .pkgcache)), "\")

## Load library
library(openMalariaUtilities)

## Load cached data
loadExperiment(\"", paste0(get(x = "experimentDir", envir = .pkgcache)), "\")

## Get scenario number to run
ID <- as.numeric(args[1])

load(file.path(get(x = \"cacheDir\", envir = openMalariaUtilities:::.pkgcache), \"scens.RData\"))
scenarios <- scens$file

cmd <- \"openMalaria\"
resources <- file.path(get(x = \"experimentDir\", envir = openMalariaUtilities:::.pkgcache))
scenario <- file.path(get(x = \"scenariosDir\", envir = openMalariaUtilities:::.pkgcache), scenarios[[ID]])
output <- file.path(
  get(x = \"outputsDir\", envir = openMalariaUtilities:::.pkgcache),
  paste0(
    sub(
      pattern = \".xml$\",
      replacement = \"\",
      basename(scenario)
    ),
    \"_out.txt\"
  )
)
ctsout <- file.path(
  get(x = \"outputsDir\", envir = openMalariaUtilities:::.pkgcache),
  paste0(
    sub(
      pattern = \".xml$\",
      replacement = \"\",
      basename(scenario)
    ),
    \"_cts.txt\"
  )
)

## Print current step
print(paste0(\"Running scenario [\", ID, \"/\", length(scenarios), \"]\"))
fullCmd <- paste0(
  cmd, \" --resource-path \", resources, \" --scenario \",
  scenario, \" --output \", output, \" --ctsout \", ctsout
)
system(command = fullCmd)
",
    file = file.path(
      get(x = "experimentDir", envir = .pkgcache), "slurm_run_simulation.R"
    ),
    sep = ""
  )
}

##' @title Submit simulation job to SLURM
##' @export
slurmRunSimulation <- function() {
  system(
    command = paste0(
      "sbatch ", file.path(
        get("experimentDir", envir = .pkgcache),
        "slurm_simulation.sh"
      )
    )
  )
}
