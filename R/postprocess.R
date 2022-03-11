### Reading and processing of the raw openMalaria files

## FIXME This is a fucking mess right now and probably error prone.

##' @title Returns a list, mapping measure numbers and names
.surveyMeasuresDict <- function() {
  ## Measures from
  ## https://github.com/SwissTPH/openmalaria/wiki/MonitoringOptions#survey-measures
  dict <- list()
  dict[["nHost"]] <- c(0)
  dict[["nInfect"]] <- c(1)
  dict[["nExpectd"]] <- c(2)
  dict[["nPatent"]] <- c(3)
  dict[["sumLogPyrogenThres"]] <- c(4)
  dict[["sumlogDens"]] <- c(5)
  dict[["totalInfs"]] <- c(6)
  dict[["nTransmit"]] <- c(7)
  dict[["totalPatentInf"]] <- c(8)

  dict[["sumPyrogenThresh"]] <- c(10)
  dict[["nTreatments1"]] <- c(11)
  dict[["nTreatments2"]] <- c(12)
  dict[["nTreatments3"]] <- c(13)
  dict[["nUncomp"]] <- c(14)
  dict[["nSevere"]] <- c(15)
  dict[["nSeq"]] <- c(16)
  dict[["nHospitalDeaths"]] <- c(17)
  dict[["nIndDeaths"]] <- c(18)
  dict[["nDirDeaths"]] <- c(19)

  dict[["nEPIVaccinations"]] <- c(20)
  dict[["allCauseIMR"]] <- c(21)
  dict[["nMassVaccinations"]] <- c(22)
  dict[["nHospitalRecovs"]] <- c(23)
  dict[["nHospitalSeqs"]] <- c(24)
  dict[["nIPTDoses"]] <- c(25)
  dict[["annAvgK"]] <- c(26)
  dict[["nNMFever"]] <- c(27)

  dict[["innoculationsPerAgeGroup"]] <- c(30)
  dict[["Vector_Nv0"]] <- c(31)
  dict[["Vector_Nv"]] <- c(32)
  dict[["Vector_Ov"]] <- c(33)
  dict[["Vector_Sv"]] <- c(34)
  dict[["inputEIR"]] <- c(35)
  dict[["simulatedEIR"]] <- c(36)
  dict[["Clinical_RDTs"]] <- c(39)

  dict[["Clinical_DrugUsage"]] <- c(40)
  dict[["Clinical_FirstDayDeaths"]] <- c(41)
  dict[["Clinical_HospitalFirstDayDeaths"]] <- c(42)
  dict[["nNewInfections"]] <- c(43)
  dict[["nMassITNs"]] <- c(44)
  dict[["nEPI_ITNs"]] <- c(45)
  dict[["nMassIRS"]] <- c(46)
  dict[["nMassVA"]] <- c(47)
  dict[["Clinical_Microscopy"]] <- c(48)
  dict[["Clinical_DrugUsageIV"]] <- c(49)

  dict[["nAddedToCohort"]] <- c(50)
  dict[["nRemovedFromCohort"]] <- c(51)
  dict[["nMDAs"]] <- c(52)
  dict[["nNmfDeaths"]] <- c(53)
  dict[["nAntibioticTreatments"]] <- c(54)
  dict[["nMassScreenings"]] <- c(55)
  dict[["nMassGVI"]] <- c(56)
  dict[["nCtsIRS"]] <- c(57)
  dict[["nCtsGVI"]] <- c(58)
  dict[["nCtsMDA"]] <- c(59)

  dict[["nCtsScreenings"]] <- c(60)
  dict[["nSubPopRemovalTooOld"]] <- c(61)
  dict[["nSubPopRemovalFirstEvent"]] <- c(62)
  dict[["nLiverStageTreatments"]] <- c(63)
  dict[["nTreatDiagnostics"]] <- c(64)
  dict[["nMassRecruitOnly"]] <- c(65)
  dict[["nCtsRecruitOnly"]] <- c(66)
  dict[["nTreatDeployments"]] <- c(67)
  dict[["sumAge"]] <- c(68)
  dict[["nInfectByGenotype"]] <- c(69)

  dict[["nPatentByGenotype"]] <- c(70)
  dict[["logDensByGenotype"]] <- c(71)
  dict[["nHostDrugConcNonZero"]] <- c(72)
  dict[["sumLogDrugConcNonZero"]] <- c(73)
  dict[["expectedDirectDeaths"]] <- c(74)
  dict[["expectedHospitalDeaths"]] <- c(75)
  dict[["expectedIndirectDeaths"]] <- c(76)
  dict[["expectedSequelae"]] <- c(77)
  dict[["expectedSevere"]] <- c(78)
  dict[["innoculationsPerVector"]] <- c(79)

  ## Custom measure summaries
  ## REVIEW This is never used later on. Why the fuck is this even here?
  ## dict[["allDeaths"]] <- c(18, 19, 19)
  ## dict[["sumUncompSev"]] <- c(14, 15, 15)
  ## dict[["allHospitalisations"]] <- c(17, 23, 24)

  return(dict)
}


##' Creates a wide dataset in post-processing
##' @param data Dataset
##' @param matchMeasureToNumber Output obtained from .surveyMeasuresDict()
##' @importFrom data.table ':='
.widenProcessedDataset <- function(data, matchMeasureToNumber) {
  ## Appease NSE notes in R CMD check
  rownum <- NULL

  ## Convert to data.table and reshape into wide format
  wideData <- data.table::dcast(
    data = data.table::data.table(data),
    formula = survey + age_group + scenario ~ measure
  )

  ## Add rownumber column
  wideData <- wideData[, rownum := seq_len(nrow(wideData))]

  ## Rename numercial measures column to humand readable name
  colnames(wideData) <- vapply(colnames(wideData), function(x) {
    ## Replace only if column name is numeric. Do the check silently.
    num <- suppressWarnings(as.numeric(x))
    if (!is.na(num)) {
      colName <- paste0(
        rownames(matchMeasureToNumber)[matchMeasureToNumber[, 1] == num],
        "_",
        num
      )
    } else {
      colName <- x
    }
    return(colName)
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)
  return(wideData)
}


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
##' @param setting variable name that specifies the setting (default: "setting")
##' @param seed_as_hist_param TODO
##' @param monthvars TODO
##' @param monthname name of dataframe (i.e. "_CombinedDat_month.RData")
##' @param monthyears which years to keep in the year.month (very wide) dataset
##' @param placeholder TODO
##' @param include TODO
##' @inheritParams write_postprocess
##' @note used often internally, never seen by user, difficult to write,
##'   sometimes breaks
##' @export
do_post_processing <- function(nameExperiment,
                               basename = "base.xml",
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
                               include = NULL) {
  ## Appease NSE notes in R CMD check
  full <- age_group <- NULL

  ### creating structure
  ## set_experiment(nameExperiment)
  ## TODO Make this a function argument
  if (file.exists(file.path(get(x = "cacheDir", envir = .pkgcache), "scens.RData")) == TRUE) {
    load(file.path(get(x = "cacheDir", envir = .pkgcache), "scens.RData"))
  } else {
    stop(paste0("File ", file.path(get(x = "cacheDir", envir = .pkgcache), "scens.RData"), " not found."))
  }

  # Lookup data
  ## REVIEW What the fuck is this shit? Why does this get transformed into a 3
  ##        column data frame? And the two last columns are never used? And
  ##        variable names get simply changed?
  eventMeasureNum <- as.list(.surveyMeasuresDict())
  match_measure_to_number <- as.data.frame(do.call("rbind", eventMeasureNum))

  #-- get better name for scens file
  scens <- as.data.frame(scens)
  scens[, "file"] <- gsub(".xml", "", scens[, "file"])

  #-- what are the output files ?
  files <- unlist(
    list.files(path = get(x = "outputsDir", envir = .pkgcache), pattern = "_out.txt", full.names = TRUE)
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
  subset_of_files <- temp$subset_of_files
  short_filename <- temp$short_filename
  one_setting <- temp$one_setting
  ## print(temp)
  ## print(full$pop)
  ## print(ORIGIN)
  #-- extract from base file # basename = "base.xml"; ORIGIN = "1918-01-01"
  tem <- .extract_base_param(get(x = "baseXml", envir = .pkgcache), SIMSTART = ORIGIN, pop = full$pop)
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
    MalariaDir = get(x = "outputsDir", envir = .pkgcache),
    loop_id = loop_id
  )
  ### transforming to a wide dataset, with each outcome as a column
  walle <- .widenProcessedDataset(alle, match_measure_to_number)

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
  if (!debugg) saveRDS(object = temp, file = file.path(get(x = "experimentDir", envir = .pkgcache), "param_names.RDS"))

  unique_variables <- temp$unique_variables
  historical_variables <- temp$historical_variables
  future_variables <- temp$future_variables

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
        get(x = "postprocessingDir", envir = .pkgcache),
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
  CombinedDat <- temp$rawdat
  outcome_variables <- temp$outcome_variables
  setting_variables <- temp$setting_variables
  model_variables <- temp$model_variables
  time_variables <- temp$time_variables
  group_variables <- temp$group_variables

  ## -- adding identifiers to it (if not already done before)
  CombinedDat <- .assign_id_variables(CombinedDat,
    unique_variables = unique_variables,
    historical_variables = historical_variables,
    future_variables = future_variables
  )

  if (!debugg) {
    save(CombinedDat, file = file.path(
      get(x = "postprocessingDir", envir = .pkgcache),
      paste0(setting_number, "_", loop_id, "_CombinedDat.RData")
    ))
  }

  ## -- in addition to aggregating to year and giving a CombinedDat_wide
  ## -- we could have a CombinedDat_month (which is by month )
  ## -- for a slightly narrower age range and outcome range
  ## -- otherwise we could have a dataset with > 12,000 columns

  ## -- we can define a 'year.month' variable if doing aggregate_to_year
  CombinedDat$year.month <- paste0(
    CombinedDat$year,
    ".",
    .twoDigitMonth(CombinedDat)
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

  CombinedDat_Aggr <- tempp$CombinedDat_Aggr
  outcome_variables <- tempp$outcome_variables

  ## -- other columns to ignore
  bads <- grep(colnames(CombinedDat_Aggr), pattern = "deploy20")
  if (length(bads) > 0) CombinedDat_Aggr <- CombinedDat_Aggr[, -bads]

  # ## Save aggregated dataset ##

  if (!debugg) {
    save(CombinedDat_Aggr, file = file.path(
      get(x = "postprocessingDir", envir = .pkgcache),
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
      file = file.path(
        get(x = "postprocessingDir", envir = .pkgcache),
        paste0(setting_number, "_", loop_id, widename)
      )
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
          file = file.path(
            get(x = "postprocessingDir", envir = .pkgcache),
            paste0(setting_number, "_", loop_id, monthname)
          )
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
