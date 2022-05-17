## Create scenarios from a base xml file with SLURM.
## The general workflow is:
##  - Get the number of scenarios and divide into equally sized batches
##  - Store this information and scenarios on disk
##  - Create SLURM launch script
##  - Create RScript doing the work


## TODO Expose the pre, cmd and post options

##' @title Run preparations for SLURM submission
##' @param expName Name of experiment
##' @param scenarios Scenario data frame
##' @param full full object
##' @param bSize Number of parrellel processed files
##' @param ntasks Number of tasks per CPU
##' @param memCPU Memory per CPU
##' @param time Maximum time
##' @param qos Quality of service
##' @export
slurmPrepareScenarios <- function(expName, scenarios, full, bSize = 200,
                                  ntasks = 1, memCPU = "150MB",
                                  time = "00:15:00", qos = "30min") {
  ## Calculate number of scenarios and divide them into packages of 200
  nscens <- as.integer(row.names(scenarios))
  batches <- splitSeq(nscens, bSize)
  nbatches <- length(batches)

  ## Store into cache
  temp <- list(
    expName = expName,
    scen_batches = batches,
    scen_nbatches = nbatches
  )
  putCache(x = "slurm", value = temp)
  putCache(x = "scenarios", value = scenarios)

  ## Create a scenario job
  filename <- file.path(
    getCache(x = "experimentDir"), "slurm_scenarios.sh"
  )
  .writeSlurm(
    jobName = paste0(expName, "_scenarios"),
    ntasks = ntasks,
    array = nbatches,
    time = time,
    qos = qos,
    output = file.path(
      file.path(getCache(x = "logsDir"), "scenarios"),
      paste0(expName, "_scenarios")
    ),
    error = file.path(
      file.path(getCache(x = "logsDir"), "scenarios"),
      paste0(expName, "_scenarios")
    ),
    pre = list(
      "module purge",
      "module load R/4.1.2-foss-2018b-Python-3.6.6"
    ),
    cmd = list(paste("Rscript", file.path(
      getCache(x = "experimentDir"), "slurm_run_scenarios.R"
    ), "$ID")),
    file = filename
  )

  ## Create R script
  cat(
    "#!/usr/bin/env Rscript

## Get arguments
args <- commandArgs(trailingOnly = TRUE)

## Set correct working directory\n",
    "setwd(dir = \"", paste0(getCache(x = "rootDir")), "\")

## Load library
library(openMalariaUtilities)

## Load cached data
loadExperiment(\"", paste0(getCache(x = "experimentDir")), "\")

## Get range of scenarios to create
slurm <- getCache(x = \"slurm\")
ID <- args[1]
rowStart <- slurm$scen_batches[[ID]][1]
rowEnd <- slurm$scen_batches[[ID]][length(slurm$scen_batches[[ID]])]

load(file.path(getCache(x = \"cacheDir\"), \"scens.RData\"))
scens <- getCache(x = \"scenarios\")

## Read placeholder names
placeholders <- names(scens)
## Select range
range <- openMalariaUtilities:::.scenariosRowSelect(
  scenarios = scens, rowStart = rowStart, rowEnd = rowEnd
)

baseFile <- getCache(x = \"baseXml\")
prefix <- getCache(x = \"experimentName\")

## Store filenames of each scenario in column
scens <- openMalariaUtilities:::.scenariosFilenames(scenarios = scens, prefix = prefix)

putCache(x = \"scenarios\", value = scens)

openMalariaUtilities:::.scenariosGenFiles(
  scenarios = scens, baseFile = baseFile, range = range,
  placeholders = placeholders, prefix = prefix
)

## Cache scenarios
storeScenarios(
  scenarios = getCache(x = \"scenarios\"),
  full = full
)",
    file = file.path(
      getCache(x = "experimentDir"), "slurm_run_scenarios.R"
    ),
    sep = ""
  )
  storeScenarios(
    scenarios = scenarios,
    full = full
  )
  .synchronizeCache(direction = "none")
}

##' @title Submit scenario creation job to SLURM
##' @export
slurmCreateScenarios <- function() {
  system(
    command = paste0(
      "sbatch ", file.path(
        getCache("experimentDir"),
        "slurm_scenarios.sh"
      )
    )
  )
}
