test_that("slurmPreparePostprocess works", {
  dir <- tempdir()
  assign(x = "experimentDir", dir, envir = openMalariaUtilities:::.pkgcache)
  assign(x = "baseDir", dir, envir = openMalariaUtilities:::.pkgcache)
  assign(x = "logsDir", file.path(dir, "logs"), envir = openMalariaUtilities:::.pkgcache)
  scenarios <- data.frame(setting = c(1:450))

  ## Bash submission script created with correct content
  slurmPreparePostprocess(
    expName = "test", scenarios = scenarios, basename = "base.xml"
  )

  expected <- paste(capture.output(cat("#!/bin/bash
#SBATCH --job-name=test_postprocess
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=2GB
#SBATCH --output=", dir, "/logs/test_postprocess_%A_%a.log
#SBATCH --error=", dir, "/logs/test_postprocess_%A_%a_error.log
#SBATCH --array=1-450
#SBATCH --time=00:06:00
#SBATCH --qos=6hours
ID=$(expr ${SLURM_ARRAY_TASK_ID} - 0)

module purge
module load R/4.1.2-foss-2018b-Python-3.6.6

Rscript ", dir, "/slurm_run_postprocess.R TRUE test 450 450 $ID

", sep = "")), sep = "", collapse = "\n")

  actual <- paste(
    capture.output(
      cat(
        readLines(file.path(dir, "slurm_postprocess.sh")),
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
load(file.path(get(x = \"cacheDir\", envir = openMalariaUtilities:::.pkgcache), \"scens.RData\"))

## Set variables
sets <- as.character(args[1])
nameExperiment <- (args[2])
## number_loops <- as.numeric(args[3])
## many <- as.numeric(args[4])
id <- as.numeric(args[5])

## Set up loops, whatever that means...

id_grid <- openMalariaUtilities:::.assign_post_process_loop(scens = scens, fsize = 200)
setting_number <- id_grid[id, \"setting_number\"]
loop_id <- id_grid[id, \"loop_id\"]

## Do the post-processing
temp <- openMalariaUtilities::do_post_processing(
  nameExperiment = nameExperiment, ORIGIN = \"1918-01-01\",
  number_loops = nrow(id_grid),
  setting_number = setting_number,
  loop_id = loop_id,
  fsize = 200,
  timeunit = c('year'),
  fut = c('fut'),
  aggregate_to_year = TRUE,
  agecats = c('2to10','0to5','All'),
  ignores = c('propOut','jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec','scaled_down_flag'),
  selectedOutVars = c('PR','nUncomp','nHost','nSevere','incidence','ddeath','edeath','edirdeath','expectedDirectDeaths','nTreatments1','nTreatments2','nTreatments3','nHospitalSeqs','nHospitalRecovs','expectedHospitalDeaths','tUncomp','tSevere'),
  widename = c('_CombinedDat_wide.RData'),
  seed_as_hist_param = TRUE,
  monthvars = c('PR'),
  monthname = c('_CombinedDat_month.RData'),
  monthyears = c(2015,2016,2017,2018,2019,2020),
  debugg = FALSE,
  placeholder = NULL,
  include = NULL
)
",
    sep = ""
  )), sep = "", collapse = "\n")

  actual <- paste(
    capture.output(
      cat(
        readLines(
          file.path(dir, "slurm_run_postprocess.R")
        ),
        sep = "\n"
      )
    ),
    sep = "", collapse = "\n"
  )

  expect_equal(actual, expected)
})


test_that("slurmPrepareCleanup works", {
  dir <- tempdir()
  assign(x = "experimentDir", dir, envir = openMalariaUtilities:::.pkgcache)
  assign(x = "baseDir", dir, envir = openMalariaUtilities:::.pkgcache)
  assign(x = "logsDir", file.path(dir, "logs"), envir = openMalariaUtilities:::.pkgcache)
  scenarios <- data.frame(setting = c(1:450))

  ## Bash submission script created with correct content
  slurmPrepareCleanup(
    expName = "test", scenarios = scenarios, basename = "base.xml"
  )

  expected <- paste(capture.output(cat("#!/bin/bash
#SBATCH --job-name=test_cleanup
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=2GB
#SBATCH --output=", dir, "/logs/test_cleanup_%A_%a.log
#SBATCH --error=", dir, "/logs/test_cleanup_%A_%a_error.log
#SBATCH --array=1-450
#SBATCH --time=00:10:00
#SBATCH --qos=30min
ID=$(expr ${SLURM_ARRAY_TASK_ID} - 0)

module purge
module load R/4.1.2-foss-2018b-Python-3.6.6

Rscript ", dir, "/slurm_run_cleanup.R $ID

", sep = "")), sep = "", collapse = "\n")

  actual <- paste(
    capture.output(
      cat(
        readLines(file.path(dir, "slurm_cleanup.sh")),
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
load(file.path(get(x = \"cacheDir\", envir = openMalariaUtilities:::.pkgcache), \"scens.RData\"))

## Set variables
setting_number <- as.numeric(args[1])

## Do the cleanup
temp <- openMalariaUtilities::do_post_process_cleanup(
  nameExperiment = test,
  make_aggr   = TRUE,
  make_wide   = TRUE,
  removefiles = FALSE,
  make_month  = FALSE,
  monthname   = c('_CombinedDat_month.RData'),
  widename    = c('_CombinedDat_wide.Rdata'),
  fut         = c('fut'),
  setting_number = setting_number,
  seed_as_hist_param =TRUE,
  placeholder = NULL,
  include = NULL
)
",
    sep = ""
  )), sep = "", collapse = "\n")

  actual <- paste(
    capture.output(
      cat(
        readLines(
          file.path(dir, "slurm_run_cleanup.R")
        ),
        sep = "\n"
      )
    ),
    sep = "", collapse = "\n"
  )

  expect_equal(actual, expected)
})
