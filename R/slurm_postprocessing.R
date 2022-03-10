## The clusterfuck continues, however this needs to work a bit longer...

##' @title helper function for batch_process
##' @param scens scens object
##' @param fsize number of files to merge
##' @param setting "setting"
##' @keywords internal
.calculate_postprocess_loops <- function(scens, fsize, setting = "setting") {
  many <- length(unique(scens[, setting]))

  ## How many files per setting
  tt <- data.frame(table(unlist(dplyr::select(scens, setting))))

  colnames(tt) <- c("setting", "files")

  ## Bbased on the max. fsize, how many loops per setting do we 'need'?
  needs <- ceiling(tt$ files / fsize)
  loops <- sum(needs) ## this many files needed ..

  return(data.frame(loops = loops, many = many))
}

##' @title Assigns the number of loops needed
##' @param scens scens object
##' @param fsize number of files to merge
##' @param setting "setting"
##' @keywords internal
.assign_post_process_loop <- function(scens, fsize, setting = "setting") {
  loop_assignment <- data.frame(table(unlist(dplyr::select(scens, setting))))

  colnames(loop_assignment) <- c("setting", "files")

  needs <- ceiling(loop_assignment$files / fsize)
  loop_assignment <- cbind(loop_assignment, needs)
  loop_assignment$ setting_number <- 1:nrow(loop_assignment)

  id_grid <- cbind.data.frame(
    ### repeating the 'setting' number 'need' number of times
    setting_number = rep(loop_assignment$ setting_number, loop_assignment$ needs),
    loop_id = c(unlist(
      ### making the loop_id go from 1:need, where need depends on the Setting
      apply(X = matrix(loop_assignment$needs), MARGIN = 1, FUN = function(x) seq(1:x))
    ))
  )

  return(id_grid)
}

##' @title Run preparations for SLURM submission of postprocess
##' @param expName Name of experiment
##' @param scenarios Scenario data frame
##' @param basename Name of base xml file
##' @param variables_of_interest Variables to store in the dataset
##' @param age_groups_of_interest Age groups to aggregate to
##' @param files_per_job Number of scenario files to have in each dataset (too
##'   many, and it will be slow)
##' @param seed_as_hist_param If true, then each seed is a different historical
##'   parameter
##' @param create_monthly_dataset If true, creates a CombinedDat_month, where
##'   each month and outcome is a column
##' @param variable_in_monthly_dataset (e.g. "PR" or "incidence")
##' @param years_in_monthly_dataset Which years to store data for
##' @param mem_process Memory for process script
##' @param time_process Time for process script
##' @param qos_process Queue for process script
##' @param ignore_these_variables Variables to ignore in post-processing
##' @param setting "setting"
##' @export
slurmPreparePostprocess <- function(expName, scenarios,
                                    basename,
                                    variables_of_interest = c(
                                      "PR", "nUncomp", "nHost", "nSevere",
                                      "incidence", "ddeath", "edeath", "edirdeath", "expectedDirectDeaths",
                                      "nTreatments1", "nTreatments2", "nTreatments3",
                                      "nHospitalSeqs", "nHospitalRecovs", "expectedHospitalDeaths",
                                      "tUncomp", "tSevere"
                                    ),
                                    age_groups_of_interest = c("2to10", "0to5", "All"),
                                    files_per_job = 200,
                                    seed_as_hist_param = TRUE,
                                    create_monthly_dataset = FALSE,
                                    variable_in_monthly_dataset = "PR",
                                    years_in_monthly_dataset = 2015:2020,
                                    mem_process = "2GB",
                                    time_process = "00:06:00",
                                    qos_process = "6hours",
                                    ignore_these_variables = c(
                                      "propOut", "jan", "feb", "mar", "apr", "may",
                                      "jun", "jul", "aug", "sep", "oct", "nov", "dec",
                                      "scaled_down_flag"
                                    ),
                                    setting = "setting") {
  widename <- "_CombinedDat_wide.RData"

  fsize <- files_per_job
  scens <- scenarios
  sets <- (length(which(colnames(scenarios) == setting)) > 0)

  if (create_monthly_dataset == TRUE) {
    message(paste("The monthly dataset will have values for:", variable_in_monthly_dataset))
    message(paste("The monthly dataset will cover the years:", paste0(years_in_monthly_dataset, collapse = ", ")))
  }

  out <- .calculate_postprocess_loops(scens, fsize)
  many <- out$many
  loops <- out$loops

  ## Create a postprocess job
  filename <- file.path(
    get(x = "experimentDir", envir = .pkgcache), "slurm_postprocess.sh"
  )
  .writeSlurm(
    jobName = paste0(expName, "_postprocess"),
    ntasks = 1,
    memCPU = mem_process,
    array = loops,
    time = time_process,
    qos = qos_process,
    output = file.path(
      get(x = "logsDir", envir = .pkgcache),
      paste0(expName, "_postprocess")
    ),
    error = file.path(
      get(x = "logsDir", envir = .pkgcache),
      paste0(expName, "_postprocess")
    ),
    pre = list(
      "module purge",
      "module load R/4.1.2-foss-2018b-Python-3.6.6"
    ),
    cmd = list(paste("Rscript", file.path(
      get(x = "experimentDir", envir = .pkgcache), "slurm_run_postprocess.R"
    ), sets, expName, loops, many, "$ID")),
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
load(file.path(get(x = \"cacheDir\", envir = openMalariaUtilities:::.pkgcache), \"scens.RData\"))

## Set variables
sets <- as.character(args[1])
nameExperiment <- (args[2])
## number_loops <- as.numeric(args[3])
## many <- as.numeric(args[4])
id <- as.numeric(args[5])

## Set up loops, whatever that means...

id_grid <- openMalariaUtilities:::.assign_post_process_loop(scens = scens, fsize = ", fsize, ")
setting_number <- id_grid[id, \"setting_number\"]
loop_id <- id_grid[id, \"loop_id\"]

## Do the post-processing
temp <- openMalariaUtilities::do_post_processing(
  nameExperiment = nameExperiment, ORIGIN = \"1918-01-01\",
  number_loops = nrow(id_grid),
  setting_number = setting_number,
  loop_id = loop_id,
  fsize = ", fsize, ",
  timeunit = ", enclose("year"), ",
  fut = ", enclose("fut"), ",
  aggregate_to_year = ", TRUE, ",
  agecats = ", enclose(age_groups_of_interest), ",
  ignores = ", enclose(ignore_these_variables), ",
  selectedOutVars = ", enclose(variables_of_interest), ",
  widename = ", enclose(widename), ",
  seed_as_hist_param = ", seed_as_hist_param, ",
  monthvars = ", enclose(variable_in_monthly_dataset), ",
  monthname = ", enclose("_CombinedDat_month.RData"), ",
  monthyears = ", enclose_numeric(years_in_monthly_dataset), ",
  debugg = ", FALSE, ",
  placeholder = ", enclose(NULL), ",
  include = ", enclose(NULL), "
)
",
    file = file.path(
      get(x = "experimentDir", envir = .pkgcache), "slurm_run_postprocess.R"
    ),
    sep = ""
  )
}

##' @title Submit postprocessing job to SLURM
##' @export
slurmRunPostprocess <- function() {
  system(
    command = paste0(
      "sbatch ", file.path(
        get("experimentDir", envir = .pkgcache),
        "slurm_postprocess.sh"
      )
    )
  )
}

##' @title Run preparations for SLURM submission of postprocessing cleanup
##' @param expName Name of experiment
##' @param scenarios Scenario data frame
##' @param basename Name of base xml file
##' @param files_per_job Number of scenario files to have in each dataset (too
##'   many, and it will be slow)
##' @param seed_as_hist_param If true, then each seed is a different historical
##'   parameter
##' @param create_monthly_dataset If true, creates a CombinedDat_month, where
##'   each month and outcome is a column
##' @param mem_clean Memory for process script
##' @param time_clean Time for process script
##' @param qos_clean Queue for process script
##' @param setting "setting"
##' @export
slurmPrepareCleanup <- function(expName, scenarios,
                                basename,
                                files_per_job = 200,
                                seed_as_hist_param = TRUE,
                                create_monthly_dataset = FALSE,
                                mem_clean = "2GB",
                                time_clean = "00:10:00",
                                qos_clean = "30min",
                                setting = "setting") {
  widename <- "_CombinedDat_wide.RData"

  fsize <- files_per_job
  scens <- scenarios
  sets <- (length(which(colnames(scenarios) == setting)) > 0)

  out <- .calculate_postprocess_loops(scens, fsize)
  many <- out$many
  loops <- out$loops

  ## Create a postprocess job
  filename <- file.path(
    get(x = "experimentDir", envir = .pkgcache), "slurm_cleanup.sh"
  )
  .writeSlurm(
    jobName = paste0(expName, "_cleanup"),
    ntasks = 1,
    memCPU = mem_clean,
    array = many,
    time = time_clean,
    qos = qos_clean,
    output = file.path(
      get(x = "logsDir", envir = .pkgcache),
      paste0(expName, "_cleanup")
    ),
    error = file.path(
      get(x = "logsDir", envir = .pkgcache),
      paste0(expName, "_cleanup")
    ),
    pre = list(
      "module purge",
      "module load R/4.1.2-foss-2018b-Python-3.6.6"
    ),
    cmd = list(paste("Rscript", file.path(
      get(x = "experimentDir", envir = .pkgcache), "slurm_run_cleanup.R"
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
load(file.path(get(x = \"cacheDir\", envir = openMalariaUtilities:::.pkgcache), \"scens.RData\"))

## Set variables
setting_number <- as.numeric(args[1])

## Do the cleanup
temp <- openMalariaUtilities::do_post_process_cleanup(
  nameExperiment = ", expName, ",
  make_aggr   = ", TRUE, ",
  make_wide   = ", TRUE, ",
  removefiles = ", FALSE, ",
  make_month  = ", create_monthly_dataset, ",
  monthname   = ", enclose("_CombinedDat_month.RData"), ",
  widename    = ", enclose("_CombinedDat_wide.RData"), ",
  fut         = ", enclose("fut"), ",
  setting_number = setting_number,
  seed_as_hist_param =", seed_as_hist_param, ",
  placeholder = ", enclose(NULL), ",
  include = ", enclose(NULL), "
)
",
    file = file.path(
      get(x = "experimentDir", envir = .pkgcache), "slurm_run_cleanup.R"
    ),
    sep = ""
  )
}

##' @title Submit postprocessing job to SLURM
##' @export
slurmRunCleanup <- function() {
  system(
    command = paste0(
      "sbatch ", file.path(
        get("experimentDir", envir = .pkgcache),
        "slurm_cleanup.sh"
      )
    )
  )
}
