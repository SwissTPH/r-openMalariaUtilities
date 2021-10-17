## Reading and processing of the raw openMalaria files

##' Function to manage postprocessing The output file is named
##' paste0(setting_number,"_",loop_id,"_CombinedDat_Aggr.RData") and is generated
##' for each scenario and setting separately
##' @param nameExperiment name of the experiment (string)
##' @param basename name of the base xml file (i.e. "base.xml")
##' @param ORIGIN date when simulation starts (yyyy-mm-dd), default =
##'   "1918-01-01"
##' @param number_loops defines how many loops to run in parallel (i.e. 12)
##' @param setting_number which loop is this one for?
##' @param loop_id which id number (sets of 500 files )
##' @param fsize how many files to combine
##' @param timeunit "year" is default
##' @param fut future variables start with 'fut'
##' @param aggregate_to_year if TRUE, it aggregates months to year
##' @param agecats defines for which age groups to postprocess, options include:
##'   c("0to5","6to11","2to10","All")
##' @param ignores variables to ignore (e.g. m1, m2, m3, m4, )
##' @param selectedOutVars variables of interest
##' @param debugg if TRUE, function runs in debugging mode (very small dataset)
##' @param widename name of dataframe (i.e. "_CombinedDat_wide.RData")
##' @param monthname name of dataframe (i.e. "_CombinedDat_month.RData")
##' @param monthyears which years to keep in the year.month (very wide) dataset
##' @param setting variable name that specifies the setting (default: "setting")
##' @inheritParams write_postprocess
##' @note used often internally, never seen by user, difficult to write,
##'   sometimes breaks
##' @export
do_post_processing <- function(nameExperiment,
                               basename,
                               ORIGIN = "1918-01-01",
                               number_loops = 12,
                               setting_number = 1,
                               loop_id = 1,
                               fsize = 500,
                               timeunit = "year",
                               fut = "fut",
                               aggregate_to_year = TRUE,
                               agecats = c("0to5", "6to11", "2to10", "All"),
                               ignores = c(
                                 "propOut", "jan", "feb", "mar", "apr", "may",
                                 "jun", "jul", "aug", "sep", "oct", "nov",
                                 "dec", "scaled_down_flag",
                                 paste0("m", 1:12), paste0("month", 1:12)
                               ),
                               selectedOutVars = c(
                                 "PR", "nUncomp", "nHost", "nSevere",
                                 "incidence", "ddeath", "edeath",
                                 "tUncomp", "tSevere", "nHosp"
                               ),
                               debugg = FALSE,
                               widename = "_CombinedDat_wide.RData",
                               setting = "setting",
                               seed_as_hist_param = TRUE,
                               monthvars = c("PR", "incidence"),
                               monthname = "_CombinedDat_month.RData",
                               monthyears = 2015:2030,
                               placeholder = NULL,
                               include = NULL,
                               env = parent.frame()) {
  ### creating structure
  ## set_experiment(nameExperiment)
  omuCache <- (get("omuCache", envir = env))
  ## TODO Make this a function argument
  load(file.path(omuCache$cacheDir, "scens.RData"))

  # Lookup data
  eventMeasureNum <- .get_om_event_measure()
  match_measure_to_number <- as.data.frame(do.call("rbind", eventMeasureNum))

  #-- get better name for scens file
  scens <- as.data.frame(scens)
  scens[, "file"] <- gsub(".xml", "", scens[, "file"])

  #-- what are the output files ?
  files <- unlist(
    list.files(path = omuCache$outputsDir, pattern = "_out.txt", full.names = TRUE)
  )
  ## print(files)
  print(paste(length(files), "_out.txt files in the output dataset"))

  if (debugg) {
    files <- files[seq_len(min(length(files), 30))]
    print("Debug mode: Combining at most 30 files")
  } # end debugg

  #-- define files of interest
  # this loop creates text files containing those outputs from the text files
  ## setting_number = 2; setting = "setting"; setting_number = 1; number_loops = 1;
  temp <- .loop_over_output_files(scens,
    setting = setting, files,
    setting_number, number_loops
  )
  subset_of_files <- temp$ subset_of_files
  short_filename <- temp$ short_filename
  one_setting <- temp$ one_setting
  ## print(temp)
  ## print(full$pop)
  ## print(ORIGIN)
  #-- extract from base file # basename = "base.xml"; ORIGIN = "1918-01-01"
  tem <- .extract_base_param(omuCache$baseXml, SIMSTART = ORIGIN, pop = full$pop)
  age_dataframe <- tem$age_dataframe
  nHost <- tem$nHost
  timestep_dataframe <- tem$timestep_dataframe

  print("Starting to combine output files")
  sets <- isTRUE("setting" %in% colnames(scens))
  # loop_id = 1; sets = T
  alle <- .shorten_output_files(
    subset_of_files = subset_of_files,
    fsize = fsize,
    sets = sets,
    one_setting = one_setting,
    setting_number = setting_number,
    MalariaDir = omuCache$outputsDir,
    loop_id = loop_id
  )

  ### transforming to a wide dataset, with each outcome as a column
  walle <- .widen_processed_dataset(alle, match_measure_to_number)

  ### --- merging with relevant scenarios.csv column names
  walle <- .merge_scens_with_outputs(walle, short_filename, scens,
    ignore = ignores
  )

  rm(alle)

  ## -- extracting parameter names
  # placeholder = NULL; include = NULL; seed_as_hist_param = T; fut = "fut"
  temp <- .extract_param_names(full, scens,
    models = "models",
    seed = "seed",
    fut = fut,
    seed_as_hist_param = seed_as_hist_param,
    placeholder = placeholder,
    include = include
  )

  #### --- saving it the first time, then loading it whenever needed
  if (!debugg) saveRDS(object = temp, file = file.path(omuCache$experimentDir, "param_names.RDS"))

  unique_variables <- temp$ unique_variables
  historical_variables <- temp$ historical_variables
  future_variables <- temp$ future_variables

  #-- ignoring age group 0
  rawdat <- subset(walle, age_group != 0)
  rm(walle)

  #-- merge with time dat and agegroup info
  rawdat <- .merge_with_age_time(
    rawdat = rawdat,
    age_dataframe = age_dataframe,
    timestep_dataframe = timestep_dataframe, nHost = nHost
  )

  ### calculating incidence information (using population size from Factorial)
  temp <- .define_outcome_variables(rawdat,
    units_of = 1e3,
    age_variables = c("age_group", "age_category"),
    model_variables = c("models", "seed", "scenario"),
    time_variables = c("survey", "Timestep", "Date", "year"),
    outVars = selectedOutVars
  )

  rawdat <- temp$rawdat
  outcome_variables <- temp$outcome_variables
  setting_variables <- temp$setting_variables
  model_variables <- temp$model_variables
  time_variables <- temp$time_variables
  group_variables <- temp$group_variables

  ## -- saving rawdat
  if (!debugg) {
    save(rawdat,
      file = file.path(
        omuCache$combinedDir,
        paste0("raw", loop_id, "_", ifelse(sets, one_setting, setting_number), ".RData")
      )
    )
  }

  if (debugg) message("After saving rawdat")

  ### --- combining different age group summaries into one dataset
  CombinedDat <- .aggregate_age_groups(
    ages = unique(rawdat$age_category),
    rawdat = rawdat,
    groups = agecats,
    group_variables = group_variables,
    outcome_variables
  )

  ### --- redoing the calculations of age-specific variables
  ### --- since they should not simply be summed over different age-groups
  temp <- .define_outcome_variables(CombinedDat,
    units_of = 1e3,
    age_variables = "age",
    model_variables = c("models", "seed", "scenario"),
    time_variables = c("survey", "Timestep", "Date", "year"),
    outVars = selectedOutVars
  )

  # debugg checks (these should be different)
  CombinedDat[1:5, c("age", "nHost_0", "nPatent_3", "incidence_999", "nSevere_15", "nUncomp_14")] %>% print()
  temp$rawdat[1:5, c("age", "nHost_0", "nPatent_3", "incidence_999", "nSevere_15", "nUncomp_14")] %>% print()

  if (debugg) message("after second defining outcomes")
  CombinedDat <- temp$ rawdat
  outcome_variables <- temp$ outcome_variables
  setting_variables <- temp$ setting_variables
  model_variables <- temp$ model_variables
  time_variables <- temp$ time_variables
  group_variables <- temp$ group_variables

  ## -- adding identifiers to it (if not already done before)
  CombinedDat <- .assign_id_variables(CombinedDat,
    unique_variables = unique_variables,
    historical_variables = historical_variables,
    future_variables = future_variables
  )

  if (!debugg) {
    save(CombinedDat, file = file.path(
      omuCache$combinedDir,
      paste0(setting_number, "_", loop_id, "_CombinedDat.RData")
    ))
  }

  ## -- in addition to aggregating to year and giving a CombinedDat_wide
  ## -- we could have a CombinedDat_month (which is by month )
  ## -- for a slightly narrower age range and outcome range
  ## -- otherwise we could have a dataset with > 12,000 columns

  ## -- we can define a 'year.month' variable if doing aggregate_to_year
  CombinedDat$ year.month <- paste0(
    CombinedDat$ year,
    ".",
    .two_digit_month(CombinedDat)
  )

  time_variables <- ifelse(aggregate_to_year, "year", "year.month")

  #-- storage for old times sake
  old_outcomeVars <- outcome_variables

  ## -- aggregation
  tempp <- .aggregate_to_year(
    CombinedDat = CombinedDat,
    age_variables = "age",
    time_variables = time_variables,
    outcome_variables = outcome_variables,
    model_variables = model_variables,
    setting_variables = setting_variables
  )

  CombinedDat_Aggr <- tempp$ CombinedDat_Aggr
  outcome_variables <- tempp$ outcome_variables

  ## -- other columns to ignore
  bads <- grep(colnames(CombinedDat_Aggr), pattern = "deploy20")
  if (length(bads) > 0) CombinedDat_Aggr <- CombinedDat_Aggr[, -bads]

  # ## Save aggregated dataset ##

  if (!debugg) {
    save(CombinedDat_Aggr, file = file.path(
      omuCache$combinedDir,
      paste0(setting_number, "_", loop_id, "_CombinedDat_Aggr.RData")
    ))
  }

  #-- creating CombinedDat_wide
  if (aggregate_to_year) timeunit <- "year" else timeunit <- "year.month"

  CombinedDat_wide <- .transform_wide_to_long(
    CombinedDat_Aggr,
    timeunit,
    selectedOutVars,
    model_variables,
    setting_variables
  )

  if (!debugg) {
    save(CombinedDat_wide,
      file = file.path(omuCache$combinedDir, paste0(setting_number, "_", loop_id, widename))
    )
  }

  ## -- creating CombinedDat_month
  if (aggregate_to_year) {
    dat <- subset(CombinedDat, CombinedDat$year %in% monthyears)
    if (nrow(dat) > 0) {
      tempp <- .aggregate_to_year(
        CombinedDat = dat,
        age_variables = "age",
        time_variables = "year.month",
        outcome_variables = old_outcomeVars,
        model_variables = model_variables,
        setting_variables = setting_variables
      )

      CombinedDat_month <- .transform_wide_to_long(
        dataframe = tempp$CombinedDat_Aggr,
        timevar = "year.month",
        outcomes_of_interest = monthvars,
        model_variables = model_variables,
        setting_variables = setting_variables
      )

      ## -- new, saving wide dataset by month
      if (!debugg) {
        save(CombinedDat_month,
          file = file.path(omuCache$combinedDir, paste0(setting_number, "_", loop_id, monthname))
        )
      }
    } else {
      warning(paste(
        "CombinedDat_month not written, because monthyears specified are not available.
     You specified:", paste0(monthyears, collapse = ", ")
      ))
    }
  } # end aggregate_to_year
  return(TRUE)
} # end .do_post_processing function
