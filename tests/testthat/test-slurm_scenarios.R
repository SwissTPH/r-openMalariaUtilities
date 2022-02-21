test_that("slurmPrepareScenarios works", {
  dir <- tempdir()
  assign(x = "experimentDir", dir, envir = openMalariaUtilities:::.pkgcache)
  assign(x = "baseDir", dir, envir = openMalariaUtilities:::.pkgcache)
  assign(x = "logsDir", file.path(dir, "logs"), envir = openMalariaUtilities:::.pkgcache)
  scenarios <- data.frame(setting = c(1:450))
  full <- list(foo = c(1:450))

  ## Bash submission script created with correct content
  slurmPrepareScenarios(expName = "test", scenarios = scenarios, full = full)

  expected <- paste(capture.output(cat("#!/bin/bash
#SBATCH --job-name=test_scenarios
#SBATCH --ntasks=1
#SBATCH --output=", dir, "/logs/test_scenarios_%A_%a.log
#SBATCH --error=", dir, "/logs/test_scenarios_%A_%a_error.log
#SBATCH --array=1-3
#SBATCH --time=00:15:00
#SBATCH --qos=30min
ID=$(expr ${SLURM_ARRAY_TASK_ID} - 0)

module purge
module load R/4.1.2-foss-2018b-Python-3.6.6

Rscript ", dir, "/slurm_run_scenarios.R $ID

", sep = "")), sep = "", collapse = "\n")

  actual <- paste(
    capture.output(
      cat(
        readLines(file.path(dir, "slurm_scenarios.sh")),
        sep = "\n"
      )
    ),
    sep = "", collapse = "\n"
  )

  expect_equal(actual, expected)

  ## Rscript created with correct content
  expected <- paste(capture.output(cat(
    "#!/usr/bin/env Rscript

## Get arguments
args <- commandArgs(trailingOnly = TRUE)

## Set correct working directory
setwd(dir = \"", dir, "\")

## Load library
library(openMalariaUtilities)

## Load cached data
loadExperiment(\"", dir, "\")

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
    sep = ""
  )), sep = "", collapse = "\n")

  actual <- paste(
    capture.output(
      cat(
        readLines(
          file.path(dir, "slurm_run_scenarios.R")
        ),
        sep = "\n"
      )
    ),
    sep = "", collapse = "\n"
  )

  expect_equal(actual, expected)
})
