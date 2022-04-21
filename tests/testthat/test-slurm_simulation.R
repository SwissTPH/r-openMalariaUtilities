test_that("slurmPrepareRunScenarios works", {
  dir <- tempdir()
  assign(x = "experimentDir", dir, envir = openMalariaUtilities:::.pkgcache)
  assign(x = "baseDir", dir, envir = openMalariaUtilities:::.pkgcache)
  assign(x = "logsDir", file.path(dir, "logs"), envir = openMalariaUtilities:::.pkgcache)
  scenarios <- data.frame(setting = c(1:450))

  ## Bash submission script created with correct content
  slurmPrepareRunScenarios(expName = "test", scenarios = scenarios)

  expected <- paste(capture.output(cat("#!/bin/bash
#SBATCH --job-name=test_simulation
#SBATCH --ntasks=1
#SBATCH --output=", dir, "/logs/simulation/test_simulation_%A_%a.log
#SBATCH --error=", dir, "/logs/simulation/test_simulation_%A_%a_error.log
#SBATCH --array=1-450
#SBATCH --time=06:00:00
#SBATCH --qos=6hours
ID=$(expr ${SLURM_ARRAY_TASK_ID} - 0)

export LMOD_DISABLE_SAME_NAME_AUTOSWAP=\"no\"
module purge
module load R/4.1.2-foss-2018b-Python-3.6.6
module load OpenMalaria/44.0-iomkl-2019.01
cd ", dir, "

Rscript ", dir, "/slurm_run_simulation.R $ID

", sep = "")), sep = "", collapse = "\n")

  actual <- paste(
    capture.output(
      cat(
        readLines(file.path(dir, "slurm_simulation.sh")),
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

## Verbose output
verbose <- NULL

## Load library
library(openMalariaUtilities)

## Load cached data
loadExperiment(\"", dir, "\")

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
  scenario, \" --output \", output, \" --ctsout \", ctsout, verbose
)
system(command = fullCmd)
",
    sep = ""
  )), sep = "", collapse = "\n")

  actual <- paste(
    capture.output(
      cat(
        readLines(
          file.path(dir, "slurm_run_simulation.R")
        ),
        sep = "\n"
      )
    ),
    sep = "", collapse = "\n"
  )

  expect_equal(actual, expected)

  ## Verbose output on
  slurmPrepareRunScenarios(expName = "test", scenarios = scenarios, verbose = TRUE)

  expected <- paste(capture.output(cat(
    "#!/usr/bin/env Rscript

## Get arguments
args <- commandArgs(trailingOnly = TRUE)

## Set correct working directory
setwd(dir = \"", dir, "\")

## Verbose output
verbose <- \" --verbose \"

## Load library
library(openMalariaUtilities)

## Load cached data
loadExperiment(\"", dir, "\")

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
  scenario, \" --output \", output, \" --ctsout \", ctsout, verbose
)
system(command = fullCmd)
",
    sep = ""
  )), sep = "", collapse = "\n")

  actual <- paste(
    capture.output(
      cat(
        readLines(
          file.path(dir, "slurm_run_simulation.R")
        ),
        sep = "\n"
      )
    ),
    sep = "", collapse = "\n"
  )

  expect_equal(actual, expected)
})
