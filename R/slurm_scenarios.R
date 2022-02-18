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
    scen_batches = batches,
    scen_nbatches = nbatches
  )
  assign(x = "slurm", value = temp, envir = .pkgcache)
  assign(x = "scenarios", value = scenarios, envir = .pkgcache)

  ## Create a scenario job
  filename <- file.path(
    get(x = "experimentDir", envir = .pkgcache), "slurm_scenarios.sh"
  )
  .writeSlurm(
    jobName = paste0(expName, "_scenarios"),
    ntasks = ntasks,
    array = nbatches,
    time = time,
    qos = qos,
    output = file.path(
      get(x = "logsDir", envir = .pkgcache),
      paste0(expName, "_scenarios")
    ),
    error = file.path(
      get(x = "logsDir", envir = .pkgcache),
      paste0(expName, "_scenarios")
    ),
    pre = list(
      "module purge",
      "module load R/4.1.2-foss-2018b-Python-3.6.6"
    ),
    cmd = list(paste("Rscript", file.path(
      get(x = "experimentDir", envir = .pkgcache), "slurm_run_scenarios.R"
    ), "$ID")),
    file = filename
  )

  ## Create R script
  cat(
    "#!/usr/bin/env Rscript

## Get arguments
args <- commandArgs(trailingOnly = TRUE)

## Set correct working directory\n",
    "setwd(dir = \"", paste0(get(x = "baseDir", envir = .pkgcache)), "\")

## Load library
library(openMalariaUtilities)

## Load cached data
loadExperiment(\"", paste0(get(x = "experimentDir", envir = .pkgcache)), "\")

## Get range of scenarios to create
slurm <- get(x = \"slurm\", envir = openMalariaUtilities:::.pkgcache)
ID <- args[1]
rowStart <- slurm$scen_batches[[ID]][1]
rowEnd <- slurm$scen_batches[[ID]][length(slurm$scen_batches[[ID]])]

load(file.path(get(x = \"cacheDir\", envir = openMalariaUtilities:::.pkgcache), \"scens.RData\"))
scens <- get(x = \"scenarios\", envir = openMalariaUtilities:::.pkgcache)

## Read placeholder names
placeholders <- names(scens)
## Select range
range <- openMalariaUtilities:::.scenariosRowSelect(
  scenarios = scens, rowStart = rowStart, rowEnd = rowEnd
)

baseFile <- get(x = \"baseXml\", envir = openMalariaUtilities:::.pkgcache)
prefix <- get(x = \"experimentName\", envir = openMalariaUtilities:::.pkgcache)

## Store filenames of each scenario in column
scens <- openMalariaUtilities:::.scenariosFilenames(scenarios = scens, prefix = prefix)

assign(x = \"scenarios\", value = scens, envir = openMalariaUtilities:::.pkgcache)

openMalariaUtilities:::.scenariosGenFiles(
  scenarios = scens, baseFile = baseFile, range = range,
  placeholders = placeholders, prefix = prefix
)

## Cache scenarios
storeScenarios(
  scenarios = get(x = \"scenarios\", envir = openMalariaUtilities:::.pkgcache),
  full = full
)",
    file = file.path(
      get(x = "experimentDir", envir = .pkgcache), "slurm_run_scenarios.R"
    ),
    sep = ""
  )
  storeScenarios(
    scenarios = scenarios,
    full = full
  )
  .storeCache()
}

##' @title Submit scenario creation job to SLURM
##' @export
slurmCreateScenarios <- function() {
  system(
    command = paste0(
      "sbatch ", file.path(
        get("experimentDir", envir = .pkgcache),
        "slurm_scenarios.sh"
      )
    )
  )
}
