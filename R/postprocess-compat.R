.get_om_event_measure <- function() {
  #' Function to get event measures names and corresponding numbers ' @note used
  # often internally, never seen by user, easy to write, never breaks

  ### measurement list
  # https://github.com/SwissTPH/openmalaria/wiki/XmlMonitoring
  eventMeasureNum <- list()
  eventMeasureNum[["nHost"]] <- (0)
  eventMeasureNum[["nInfect"]] <- c(1)
  eventMeasureNum[["nExpectd"]] <- c(2)
  eventMeasureNum[["nPatent"]] <- c(3)
  eventMeasureNum[["sumlogDens"]] <- c(5)
  eventMeasureNum[["totalInfs"]] <- c(6)
  eventMeasureNum[["nTransmit"]] <- c(7)
  eventMeasureNum[["totalPatentInf"]] <- c(8)
  eventMeasureNum[["removed"]] <- c(9)
  #
  eventMeasureNum[["sumPyrogenThresh"]] <- c(10)
  eventMeasureNum[["nTreatments1"]] <- c(11)
  eventMeasureNum[["nTreatments2"]] <- c(12)
  eventMeasureNum[["nTreatments3"]] <- c(13)
  eventMeasureNum[["nUncomp"]] <- c(14)
  eventMeasureNum[["nSevere"]] <- c(15)
  eventMeasureNum[["nSeq"]] <- c(16)
  eventMeasureNum[["nHospitalDeaths"]] <- c(17)
  eventMeasureNum[["nIndDeaths"]] <- c(18)
  eventMeasureNum[["nDirDeaths"]] <- c(19)
  #
  eventMeasureNum[["nEPIVaccinations"]] <- c(20)
  eventMeasureNum[["allCauseIMR"]] <- c(21)
  eventMeasureNum[["nMassVaccinations"]] <- c(22)
  eventMeasureNum[["nHospitalRecovs"]] <- c(23)
  eventMeasureNum[["nHospitalSeqs"]] <- c(24)
  eventMeasureNum[["nIPTDoses"]] <- c(25)
  eventMeasureNum[["annAvgK"]] <- c(26)
  eventMeasureNum[["nNMFever"]] <- c(27)
  eventMeasureNum[["removed"]] <- c(28)
  eventMeasureNum[["removed"]] <- c(29)
  #
  eventMeasureNum[["innoculationsPerAgeGroup"]] <- c(30)
  eventMeasureNum[["Vector_Nv0"]] <- c(31)
  eventMeasureNum[["Vector_Nv"]] <- c(32)
  eventMeasureNum[["Vector_Ov"]] <- c(33)
  eventMeasureNum[["Vector_Sv"]] <- c(34)
  eventMeasureNum[["inputEIR"]] <- c(35)
  eventMeasureNum[["simulatedEIR"]] <- c(36)
  eventMeasureNum[["removed"]] <- c(37)
  eventMeasureNum[["removed"]] <- c(38)
  eventMeasureNum[["Clinical_RDTs"]] <- c(39)
  #
  eventMeasureNum[["Clinical_DrugUsage"]] <- c(40)
  eventMeasureNum[["Clinical_FirstDayDeaths"]] <- c(41)
  eventMeasureNum[["Clinical_HospitalFirstDayDeaths"]] <- c(42)
  eventMeasureNum[["nNewInfections"]] <- c(43)
  eventMeasureNum[["nMassITNs"]] <- c(44)
  eventMeasureNum[["nEPI_ITNs"]] <- c(45)
  eventMeasureNum[["nMassIRS"]] <- c(46)
  eventMeasureNum[["nMassVA"]] <- c(47)
  eventMeasureNum[["Clinical_Microscopy"]] <- c(48)
  eventMeasureNum[["Clinical_DrugUsageIV"]] <- c(49)
  #
  eventMeasureNum[["nAddedToCohort"]] <- c(50)
  eventMeasureNum[["nRemovedFromCohort"]] <- c(51)
  eventMeasureNum[["nMDAs"]] <- c(52)
  eventMeasureNum[["nNmfDeaths"]] <- c(53)
  eventMeasureNum[["nAntibioticTreatments"]] <- c(54)
  eventMeasureNum[["nMassScreenings"]] <- c(55)
  eventMeasureNum[["nMassGVI"]] <- c(56)
  eventMeasureNum[["nCtsIRS"]] <- c(57)
  eventMeasureNum[["nCtsGVI"]] <- c(58)
  eventMeasureNum[["nCtsMDA"]] <- c(59)
  #
  eventMeasureNum[["nCtsScreenings"]] <- c(60)
  eventMeasureNum[["nSubPopRemovalTooOld"]] <- c(61)
  eventMeasureNum[["nSubPopRemovalFirstEvent"]] <- c(62)
  eventMeasureNum[["nLiverStageTreatments"]] <- c(63)
  eventMeasureNum[["nTreatDiagnostics"]] <- c(64)
  eventMeasureNum[["nMassRecruitOnly"]] <- c(65)
  eventMeasureNum[["nCtsRecruitOnly"]] <- c(66)
  eventMeasureNum[["nTreatDeployments"]] <- c(67)
  eventMeasureNum[["sumAge"]] <- c(68)
  eventMeasureNum[["nInfectByGenotype"]] <- c(69)
  #
  eventMeasureNum[["nPatentByGenotype"]] <- c(70)
  eventMeasureNum[["logDensByGenotype"]] <- c(71)
  eventMeasureNum[["nHostDrugConcNonZero"]] <- c(72)
  eventMeasureNum[["sumLogDrugConcNonZero"]] <- c(73)
  eventMeasureNum[["expectedDirectDeaths"]] <- c(74)
  eventMeasureNum[["expectedHospitalDeaths"]] <- c(75)
  eventMeasureNum[["expectedIndirectDeaths"]] <- c(76)
  eventMeasureNum[["expectedSequelae"]] <- c(77)
  eventMeasureNum[["expectedSevere"]] <- c(78)
  ## REVIEW This is not defined in the openMalaria spec
  eventMeasureNum[["allDeaths"]] <- c(18, 19, 19)
  eventMeasureNum[["sumUncompSev"]] <- c(14, 15, 15)
  eventMeasureNum[["allHospitalisations"]] <- c(17, 23, 24)

  return(eventMeasureNum)
}

#' returns a list of files
#' @param scens scens object
#' @param setting variable name that specifies the setting to run (default 'setting')
#' @param files list of files to loop over
#' @param setting_number which department/Setting number
#' @param number_loops how many loops in total
.loop_over_output_files <- function(scens # scenario file
                                    , setting = "setting",
                                    files # list of files
                                    , setting_number # which district?
                                    , number_loops # how many loops
) {
  one_setting <- NULL

  # loop over setting (if sets = T)
  sets <- isTRUE(setting %in% colnames(scens))
  if (sets) {
    sett <- sort(unique(scens[, setting]))
    one_setting <- sett[setting_number]
    these_files <- which(scens[, setting] == one_setting)

    print(paste0(length(these_files), " scenarios with Setting = ", one_setting))

    short_filename <- unique(c(gsub(".xml", "", scens$file[these_files])))
    subset_of_files <- paste0(short_filename, "_out.txt")

    ### do these_files subfiles actually exist in file?
    actual <- which(is.element(subset_of_files, unique(
      c(gsub(".*/", "", as.character(files)))
    )))

    print(paste0(
      length(actual),
      " files found of the ", length(subset_of_files), " requested."
    ))

    if (length(actual) != length(subset_of_files)) {
      warning(paste(
        "ACHTUNG:", length(actual), "files found out of",
        length(subset_of_files), "specified"
      ))
    }

    subset_of_files <- subset_of_files[actual]
    short_filename <- short_filename[actual]
    these_files <- these_files[actual]
    print(paste0(length(subset_of_files), " files available to join together."))
  }

  # loop over numeric order
  if (!sets) {
    many <- ceiling(length(files) / number_loops)
    these_files <- (1 + many * (setting_number - 1)):ifelse(
      setting_number < number_loops, many * setting_number, length(files)
    )
    print(length(these_files))

    subset_of_files <- unique(
      c(gsub(".*/", "", as.character(files)[these_files]))
    )
    short_filename <- gsub("_out.txt", "", subset_of_files)
  } # end numeric loop

  return(list(
    subset_of_files = subset_of_files,
    short_filename = short_filename,
    these_files = these_files,
    one_setting = one_setting
  ))
}

#' Supplementary r-script to be included by others, to load key information from
#' base xml
#' @param MainDir Path to experiment directory (character)
#' @param SIMSTART Starting date of the simulations in the format "yyyy-mm-dd",
#'   default e.g. "1918-01-01", If NULL the base xml is searched for the term
#'   "Simulation Start" or surveys number 1
#' @param basename name of the base xml file
#' @param pop Total population (integer), if NULL, it will be read from the xml
#'   file
#' @note used often internally, never seen by user, easy to write, never breaks
#' @importFrom xml2 xml_find_all read_xml xml_integer xml_children
.extract_base_param <- function(baseXml = NULL,
                                SIMSTART = "1918-01-01",
                                pop = NULL) {
  if (!file.exists(file.path(baseXml))) stop("Base xml file not found")

  #############  BASE  --- GET child nodes
  base <- xml2::read_xml(file.path(baseXml))
  demography <- xml2::xml_find_all(base, ".//demography")
  ageGroup <- xml2::xml_find_all(base, ".//ageGroup")
  surveysXML <- xml2::xml_find_all(base, ".//surveys")

  ## get nHost
  ## assumes that BASE.XML has a specified value (not experiment varying popsize)
  nHost <- strsplit(as.character(demography), " ")[[1]][4]


  if (length(grep(nHost, pattern = "@pop@")) > 0) {
    nHost <- pop
  } else {
    nHost <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", nHost)
    nHost <- as.numeric(gsub("popSize", "", nHost))
    nHost
  }

  ## ---sometimes full$pop may be empty...
  if (is.null(pop)) pop <- nHost

  ## get AgeGroups  (needs to be modified depending on the number of age groups)
  cat(strsplit(as.character(ageGroup), " ")[[2]])
  ageCutOffs <- strsplit(as.character(ageGroup), "[^[:digit:]]")[[2]]

  ## convert strings to numeric ("" become NA)
  ageCutOffs <- as.numeric(unlist(ageCutOffs))

  ## remove NA and duplicates
  ageCutOffs <- unique(ageCutOffs[!is.na(ageCutOffs)])
  nAgeGroups <- as.matrix(c(1:(length(ageCutOffs) - 1)))

  ages <- NULL
  for (jj in 2:length(ageCutOffs)) {
    agec <- paste0(ageCutOffs[jj - 1], "-", ageCutOffs[jj])
    ages <- c(ages, agec)
  }
  age_category <- matrix(ages)
  rm(ages)

  age_dataframe <- as.data.frame(cbind(nAgeGroups, age_category))

  colnames(age_dataframe) <- c("nAgeGroups", "age_category")

  ## get timesteps and number of surveys
  if (is.null(SIMSTART)) {
    SIMSTART <- omuCache$surveyTimes$date[[1]]
  }

  timesteps <- omuCache$surveyTimes$timestep # timesteps
  nsurveys <- length(omuCache$surveyTimes$timestep)
  timedates <- as.Date(omuCache$surveyTimes$date)
  timestep_dataframe <- cbind(as.data.frame(timesteps), as.data.frame(timedates))

  timestep_dataframe$monitoringStep <- rownames(timestep_dataframe)
  colnames(timestep_dataframe) <- c("Timestep", "Date", "monitoringStep")
  timestep_dataframe$year <- as.numeric(format(timestep_dataframe$Date, "%Y"))

  colnames(timestep_dataframe)[colnames(timestep_dataframe) == "monitoringStep"] <- "survey"
  timestep_dataframe$survey <- as.numeric(timestep_dataframe$ survey)

  if (max(timestep_dataframe$year) < 2000
  ) {
    warning("The maximum year is < 2000, did you specifiy the correct SIMSTART?")
  }

  return(list(
    age_dataframe = age_dataframe,
    timedates = timedates,
    timesteps = timesteps,
    timestep_dataframe = timestep_dataframe,
    nHost = nHost,
    SIMSTART = SIMSTART
  ))
}

.shorten_output_files <- function(subset_of_files,
                                  fsize = 100,
                                  sets = T, one_setting, setting_number = 1,
                                  MalariaDir,
                                  loop_id = 1) {
  #' returns 'alle' with columns := survey, age_group,measure,value,scenario
  #' @param subset_of_files list of files
  #' @param fsize maximum number of files to combine
  #' @param sets T or F if scens has setting column
  #' @param one_setting name of setting variable
  #' @param setting_number Setting number index
  #' @param loop_id loop_id for each 500 files
  #' @param MalariaDir directory of output files
  #' @note used often internally, never seen by user, medium to write, never breaks

  alle <- NULL
  tempname <- file.path(
    MalariaDir, paste0("temp", ifelse(sets, one_setting, setting_number), "_", loop_id, ".txt")
  )
  print(tempname)

  ## -- removing if something already there
  unlink(tempname)

  some <- ((loop_id - 1) * fsize + 1):min(length(subset_of_files), (loop_id * fsize))
  print(paste0(length(some), " files to be concatenated"))

  ### Ensuring that we don't add files that don't exist ...
  add_these <- subset_of_files[some]
  bads <- which(is.na(add_these))

  if (length(bads) > 0) {
    message(paste(
      "CAUTION:", length(bads),
      "of these files don't exist. We shall ignore them."
    ))
    add_these <- add_these[-bads]
  }

  ### write files to one, using windows
  if (length(add_these) > 0) file.append(file1 = tempname, file2 = file.path(MalariaDir, add_these))

  ### filename
  alld <- utils::read.table(paste0(tempname))
  colnames(alld) <- c("survey", "age_group", "measure", "value")

  ## removing non-numeric age groups
  ## to speed up code going forward
  bads <- which(nchar(as.character(alld[, "age_group"])) > 8)
  if (length(bads) > 0) alld <- alld[-bads, ]
  print(paste(length(bads), "rows removed from dataset."))

  ## removing "survey = 1" instances
  bads <- which(alld[, "survey"] == 1)
  if (length(bads) > 0) alld <- alld[-bads, ]
  print(paste(length(bads), "survey = 1 instances removed"))

  ## adding filename column for identification
  ## --- based on first instance of "smallest" survey number
  first <- which(alld$ survey == alld$ survey[1] &
    alld$ age_group == alld$ age_group[1] &
    alld$ measure == alld$ measure[1])

  ## --- this gives the start/stop of each of the file names
  sstop <- cbind(first, c(first[-1] - 1, nrow(alld)))
  nname <- data.frame(matrix(NA, nrow = nrow(alld), ncol = 1))
  colnames(nname) <- "file"

  for (jk in seq_len(nrow(sstop))) { # jk = 1
    nname[sstop[jk, 1]:sstop[jk, 2], 1] <- gsub(
      x = subset_of_files[some][jk], pattern = "_out.txt", replacement = ""
    )
  } # end loop

  alle <- rbind(alle, cbind(alld, nname))

  print(tempname)
  file.remove(tempname)

  # }# end loop

  colnames(alle) <- c("survey", "age_group", "measure", "value", "scenario")
  if (dim(alle)[1] < 2) stop("alle needs to be a matrix - for some reason, it is not.")

  return(alle)
}

.widen_processed_dataset <- function(alle, match_measure_to_number) {
  #' creates a wide dataset in post-processing
  #' @param alle dataset
  #' @importFrom magrittr %>%
  #' @param match_measure_to_number output obtained from .get_om_event_measure()
  #' @note used often internally, never seen by user, easy to write, never breaks
  #' @importFrom tidyr spread
  #' @importFrom dplyr group_by


  walle <- data.frame(alle) %>%
    tidyr::spread(measure, value) %>%
    dplyr::group_by(scenario)

  walle[, "rownum"] <- seq_len(nrow(walle))

  ## the columns are "survey","age_group","scenario","0","1","3", etc
  ## and this code will loop only through the columns that aren't numbers
  print("begin loop through columns")
  jj <- which(!is.na(as.numeric(colnames(walle))))
  for (i in jj) {
    t_measureNum <- as.numeric(colnames(walle)[i])
    t_measureName <- rownames(match_measure_to_number)[
      which(
        match_measure_to_number[, 1] == t_measureNum
      )
    ]

    colnames(walle)[i] <- paste0(t_measureName, "_", t_measureNum)
    print(colnames(walle)[i])

    rm(t_measureNum, t_measureName)
  } # end column loop

  return(walle)
}

.merge_scens_with_outputs <- function(walle, short_filename, scens,
                                      ignore = c(
                                        "propOut", "jan", "feb", "mar", "apr", "may",
                                        "jun", "jul", "aug", "sep", "oct", "nov", "dec",
                                        "scaled_down_flag"
                                      )) {
  #' merging walle with scenarios columns
  #' merge "files_out" list (in that order) with the variable values
  #' specified in the scenarios.csv file
  #' @param walle wide object
  #' @param short_filename "wutest_1" "wutest_2", short in that it does not have ".xml" at end
  #' @param scens dataset of scenarios
  #' @param ignore variables to ignore
  #' @note used often internally, never seen by user, easy to write, never breaks

  ids <- matrix(short_filename, ncol = 1)
  colnames(ids) <- "file"

  ### --- not merging the IRSdeploy dates and jan,feb,mar, ... values
  # ignore = NULL
  bads <- NULL
  if (length(ignore) > 0) {
    for (i in seq_len(length(ignore))
    ) {
      bads <- c(bads, grep(colnames(scens), pattern = ignore[i]))
    }
    bads <- unique(bads)
  } # end loop

  if (length(bads) > 0) scens <- scens[, -bads]

  ## keeping only the scenarios stored in the "ids" list
  scen_merge <- merge(ids, scens, by = "file", all.x = T)

  ## merging the scenario information with the OM output (all_out.txt)
  colnames(scen_merge)[which(colnames(scen_merge) == "file")] <- "scenario"

  walle <- merge(walle, scen_merge, by = "scenario", all.x = T)

  return(walle)
} # end function .merge_scens_with_outputs

.define_outcome_variables <- function(rawdat,
                                      units_of = 1e3,
                                      age_variables = c("age_group", "age_category"),
                                      model_variables = c("models", "seed", "scenario"),
                                      time_variables = c("survey", "Timestep", "Date", "year"),
                                      outVars = selectedOutVars) {

  #' Define outcome variables
  #' @param rawdat dataframe of raw simulation outputs
  #' @param units_of integer value (e.g. 1e3 for units of 1000s )
  #' @param age_variables  names of the variables that denote age in the population, usually includes "age_group","age_category"
  #' @param model_variables names of the basic model variables, usually includes "models","seed","scenario"
  #' @param time_variables names of the variables that denote time, usually includes "survey","Timestep","Date","year"
  #' @param outVars names of the outcome variables
  #' @note used often internally, never seen by user, medium to write, never breaks
  ### removing models if it isn't defined?

  if (length(which(colnames(rawdat) %in% "models")) < 1) {
    rawdat[, "models"] <- "base"
  }
  if (length(which(colnames(rawdat) %in% "seed")) < 1) {
    rawdat[, "seed"] <- 0
  }

  ## defines incidence; edeath & ddeath (mortality)
  ## units_of is cases per 1'000; units_of = 1000
  rawdat[, "pop"] <- as.numeric(rawdat[, "pop"])

  ### defining incidence
  if ("incidence" %in% outVars) {
    rawdat <- .define_new_outcome_variable(
      dat = rawdat,
      newvar = "incidence_999",
      sumvar = c("nUncomp_14", "nSevere_15"),
      divideby = "nHost_0",
      units_of = units_of
    )
  }

  ### defining number of uncomplicated and severe treatments
  if ("tUncomp" %in% outVars) {
    rawdat <- .define_new_outcome_variable(rawdat,
      newvar = "tUncomp_755",
      sumvar = c("nTreatments1_11", "nTreatments2_12"),
      divideby = NULL,
      units_of = 1
    )
  } # end tUncomp

  if ("tSevere" %in% outVars) {
    rawdat <- .define_new_outcome_variable(rawdat,
      newvar = "tSevere_756",
      sumvar = c("nTreatments3_13"),
      divideby = NULL,
      units_of = 1
    )
  } # end tSevere

  ### defining nHospitalizations
  if ("nHosp" %in% outVars) {
    rawdat <- .define_new_outcome_variable(rawdat,
      newvar = "nHosp_144",
      sumvar = c(
        "nHospitalDeaths_17",
        "nHospitalRecovs_23",
        "nHospitalSeqs_24"
      ),
      divideby = NULL,
      units_of = 1
    )
  }

  ### defining mortality
  if ("edeath" %in% outVars) {
    rawdat <- .define_new_outcome_variable(rawdat,
      newvar = "edeath_888",
      sumvar = c(
        "expectedDirectDeaths_74",
        "expectedIndirectDeaths_76"
      ),
      divideby = "nHost_0",
      units_of = 1e5
    )

    ## direct deaths (expected)
    rawdat <- .define_new_outcome_variable(rawdat,
      newvar = "edirdeath_881",
      sumvar = c(
        "expectedDirectDeaths_74"
      ),
      divideby = "nHost_0",
      units_of = 1e5
    )
  }

  if ("ddeath" %in% outVars) {
    rawdat <- .define_new_outcome_variable(
      dat = rawdat,
      newvar = "ddeath_777",
      sumvar = c("nIndDeaths_18", "nDirDeaths_19"),
      divideby = "nHost_0",
      units_of = 1e5
    )
  }

  ### -- removing @ symbols from column names
  colnames(rawdat) <- gsub("@", "", colnames(rawdat))

  ## specifying the outcome variables
  outcome_variables <- colnames(rawdat)[grep("_[0-9]", colnames(rawdat))]
  outcome_variables

  setting_variables <- colnames(rawdat)[setdiff(which(
    !(colnames(rawdat) %in%
      c(model_variables, time_variables, outcome_variables, age_variables, "rownum"))
    ### removing the X.column names (from @hist2002@ @hist2003@ etc)
  ), grep(x = colnames(rawdat), pattern = "@"))]

  print(setting_variables)
  print(model_variables)
  print(time_variables)

  group_variables <- c(time_variables, setting_variables, model_variables)
  rawdat[outcome_variables] <- lapply(rawdat[outcome_variables], as.numeric)


  return(list(
    rawdat = rawdat,
    outcome_variables = outcome_variables,
    setting_variables = setting_variables,
    model_variables = model_variables,
    time_variables = time_variables,
    group_variables = group_variables
  ))
} # end .define_outcome_variables

.merge_with_age_time <- function(rawdat, age_dataframe, timestep_dataframe, nHost) {
  #' add age group information to dataset
  #' @param rawdat dataset
  #' @param nHost variable of host
  #' @param age_dataframe created from .extract_base_param()
  #' @param timestep_dataframe created from .extract_base_param()
  #' @importFrom dplyr left_join
  #' @note used often internally, never seen by user, easy to write, never breaks
  ageLabels <- age_dataframe$ age_category
  ageNr <- age_dataframe$ nAgeGroups
  rawdat$ age_category <- factor(rawdat$ age_group, levels = ageNr, labels = ageLabels)

  table(rawdat$age_group, rawdat$age_category)

  # does this resolve the issue of having removing survey == 1 ?
  rawdat <- subset(rawdat, rawdat$ survey > 1)
  timestep_dataframe <- subset(timestep_dataframe, timestep_dataframe$ survey > 1)

  ### add time information to dataset
  rawdat$ survey <- as.numeric(rawdat$ survey)
  timestep_dataframe$ survey <- as.numeric(timestep_dataframe$ survey)

  ## testing if all surveys correspond to each other
  which(sort(unique(rawdat$survey)) != sort(timestep_dataframe$survey))

  rawdat <- dplyr::left_join(rawdat, timestep_dataframe, by = "survey", all.x = TRUE)

  #-- in case 'pop' isn't defined ...
  if (length(grep(colnames(rawdat), pattern = "pop")) < 1) rawdat[, "pop"] <- nHost

  return(rawdat)
} # end .merge_with_age_time

### for creating variables like incidence, prevalence, tSevere, tUncomp
.define_new_outcome_variable <- function(dat, newvar, sumvar, divideby = NULL, units_of = 1) {
  #' Function to calculate new outcome variables
  #' @param dat rawdat
  #' @param newvar variable name to create
  #' @param sumvar variables to sum over
  #' @param divideby divide by (column name)
  #' @param units_of report in units of units_of (1000)
  #' @importFrom magrittr %>%
  #' @importFrom utils head
  #' @note used often internally, never seen by user, easy to write, never breaks

  #-- needs to be non-negative integer
  stopifnot(units_of > 0)
  stopifnot(!is.integer(units_of))

  if (sum(sumvar %in% colnames(dat)) < length(sumvar)
  ) {
    stop(paste0(sumvar, " not within the dataset, cannot create ", newvar))
  }

  if (!is.null(divideby)) div <- dat[, divideby] else div <- 1


  ## checking that these sumvars are even in the dataset
  if (length(which(colnames(dat) %in% sumvar)) > 0) {
    if (length(sumvar) > 1
    ) {
      dat[, newvar] <- signif(rowSums(dat[, sumvar]) / div * units_of, 5)
    } else {
      dat[, newvar] <- signif(dat[, sumvar] / div * units_of, 5)
    }
  } # end at least something to sum over

  return(dat)
} # end function

.aggregate_age_groups <- function(ages, rawdat,
                                  groups = c("0to5", "6to11", "2to10", "All"),
                                  group_variables,
                                  outcome_variables) {
  #' generate age sets..
  #' @param ages vector of age labels
  #' @param rawdat rawdat object
  #' @param groups default groups used  "0to5","2to10","6to11","All"
  #' @param group_variables example could be "age" "year" "setting"
  #' @param outcome_variables "PR" or "incidence"
  #' @importFrom magrittr %>% %<>%
  #' @importFrom tidyr separate
  #' @importFrom dplyr mutate
  #' @note used often internally, never seen by user, easy to write, never breaks
  # groups = c("0to5","2to10","6to11","All")
  group_name <- groups

  ## -- the separator variable will be 'to'
  bads <- which(groups == "All")
  if (length(bads) > 0) groups[bads] <- c("0to200")

  ## --- specifying the age groups of interest
  groups <- data.frame(groups) %>%
    tidyr::separate(col = "groups", sep = "to", into = c("lo", "hi")) %>%
    unlist() %>%
    as.numeric() %>%
    matrix(ncol = 2)

  ## -- which 'age' groups do those correspond to?
  temp <- data.frame(ages) %>%
    tidyr::separate(col = "ages", sep = "-", into = c("lo", "hi")) %>%
    unlist() %>%
    as.numeric() %>%
    matrix(ncol = 2)

  rawdat_agg <- NULL

  for (i in seq_len(nrow(groups))) { # i=2

    these_ages <- as.character(ages[which(temp[, 1] >= groups[i, 1] & temp[, 2] <= groups[i, 2])])

    ## -- this needs to account for age-weighting?
    rawdat_temp <- .sum_over_age_groups(
      rawdat, these_ages, group_name[i],
      group_variables, outcome_variables
    )

    rawdat_agg <- rbind(rawdat_agg, rawdat_temp)
  } # end loop over groups

  rawdat_agg %<>%
    dplyr::mutate(year = as.numeric(year)) %>%
    as.data.frame()

  return(rawdat_agg)
} # end function

.sum_over_age_groups <- function(rawdat,
                                 age_range,
                                 age_group_character,
                                 group_variables,
                                 outcome_variables) {
  #' Collate results per  age group
  #' @param rawdat rawdata dataset
  #' @param age_range "0-1" as example, dash is important
  #' @param age_group_character "0to1" "All" as examples
  #' @param group_variables group variables (year, setting, etc)
  #' @param outcome_variables variables of interest
  #' @importFrom dplyr filter group_by_ summarize_at mutate
  #' @note used often internally, never seen by user, easy to write, never breaks
  # age_range <- c("0-1")
  # age_group_character  <- "0to1"
  # dataframe <- rawdat

  #-- ignoring 'ageprop' as a grouping variable
  bads <- which(group_variables == "ageprop")
  if (length(bads) > 0) group_variables <- group_variables[-bads]

  tempdat <- rawdat %>%
    dplyr::filter(age_category %in% age_range) %>%
    dplyr::group_by_(.dots = group_variables) %>%
    ## -- we should recalculate the variables that are divided by nHost
    ## -- since they are age-specific, and cannot be summed directly
    dplyr::summarise_at(outcome_variables, sum, na.rm = TRUE) %>%
    dplyr::mutate(age = age_group_character) %>%
    as.data.frame()


  return(tempdat)
} # end function

.aggregate_to_year <- function(CombinedDat, age_variables = "age",
                               time_variables = "year",
                               outcome_variables,
                               model_variables, setting_variables) {

  #' aggregate survey to year - (depending on frequency of surveys timesteps monitored)
  #' @param CombinedDat dataset
  #' @param age_variables "age"
  #' @param time_variables "year"
  #' @param outcome_variables "PR"
  #' @param model_variables model_variables = c("models", "seed", "scenario")
  #' @param setting_variables "setting"
  #' @importFrom magrittr %>%
  #' @importFrom dplyr group_by_ summarize_at mutate
  #' @note used often internally, never seen by user, medium to write, never breaks

  #-- avoiding 'no visible binding' warnings
  nPatent_3 <- nHost_0 <- NULL

  group_variables <- c(model_variables, time_variables, age_variables, setting_variables)

  CombinedDat_Aggr <- CombinedDat %>%
    dplyr::group_by_(.dots = group_variables) %>%
    dplyr::summarise_at(outcome_variables, sum, na.rm = TRUE) %>%
    dplyr::mutate(
      PR = signif(nPatent_3 / nHost_0, 5) #* 100
      , aggrMethod = "sum of months"
    ) %>%
    as.data.frame()

  ## add constant age groups
  ageHostDat <- CombinedDat %>%
    dplyr::group_by_(age_variables) %>%
    dplyr::summarize(nHost_0_fixed = mean(nHost_0)) %>%
    unique() %>%
    as.data.frame()

  CombinedDat_Aggr$ nHost_0_fixed <- factor(CombinedDat_Aggr[, age_variables],
    levels = ageHostDat[, age_variables],
    labels = ageHostDat$nHost_0_fixed
  )

  CombinedDat_Aggr$ nHost_0_fixed <- as.numeric(
    as.character(CombinedDat_Aggr$nHost_0_fixed)
  )

  table(CombinedDat_Aggr$ nHost_0_fixed,
    CombinedDat_Aggr[, age_variables],
    exclude = NULL
  )

  ## -- removing excess numbers from variable names
  colnames(CombinedDat_Aggr) <- gsub("_.[0-9]*$", "", colnames(CombinedDat_Aggr))
  outcome_variables <- gsub("_.[0-9]*$", "", outcome_variables)

  ## removing duplicate nHost columns
  bads <- which(colnames(CombinedDat_Aggr) == "nHost")
  if (length(bads) > 0) {
    CombinedDat_Aggr <- CombinedDat_Aggr[, -bads]
  }

  ### --- ageprop and nHost correction
  CombinedDat_Aggr$ nHost <- round(CombinedDat_Aggr$ nHost_0_fixed)
  CombinedDat_Aggr$ ageprop <- round(CombinedDat_Aggr$ nHost
    / CombinedDat_Aggr$ pop, 4)

  ## ---nPatent correction (it was summed to months before)
  CombinedDat_Aggr$ nPatent <- NULL
  CombinedDat_Aggr$ nPatent <- round(CombinedDat_Aggr$ PR * CombinedDat_Aggr$ nHost)


  return(list(
    CombinedDat_Aggr = CombinedDat_Aggr,
    outcome_variables = outcome_variables
  ))
} # end aggregate code

.two_digit_month <- function(CombinedDat) {
  #' uses the month of a date to create a two digit month (character)
  #' @param CombinedDat CombinedDat dataset
  #' @note used internally, never breaks, never seen by user
  #' @importFrom lubridate ymd month

  # CombinedDat = data.frame( Date = c("1918-01-01","1918-02-01"))

  months <- lubridate::month(lubridate::ymd(as.Date(CombinedDat$ Date)))
  months <- stats::na.omit(months)
  stopifnot(max(months, na.rm = TRUE) <= 12)
  single_digit <- (months <= 9)
  months[single_digit] <- paste0("0", months[single_digit])
  months[!single_digit] <- as.character(months[!single_digit])

  return(months)
} # end function

.transform_wide_to_long <- function(dataframe,
                                    timevar,
                                    outcomes_of_interest = outcome_variables,
                                    model_variables,
                                    setting_variables) {
  #' Transform dataset from wide to long format
  #' @param dataframe default CombinedDat_Aggr
  #' @param timevar default timeunit
  #' @param outcomes_of_interest default  selectedOutVars
  #' @param model_variables model_variables = c("models", "seed", "scenario")
  #' @param setting_variables example is "setting"
  #' @importFrom tidyr unite_ separate
  #' @importFrom dplyr select_ group_by
  #' @note used often internally, never seen by user, medium to write, never breaks

  bads <- which(!is.element(outcomes_of_interest, colnames(dataframe)))

  ## -- we should remove them from our required list of outcome_variables
  if (length(bads) > 0) {
    outcomes_of_interest <- outcomes_of_interest[-bads]
    print("ACHTUNG: Post-Processing only for these variables")
    print(outcomes_of_interest)
  } # end fewer variables

  time_variables <- timevar
  print(sum(duplicated(colnames(dataframe))))
  print("duplicated columns in dataset")

  dataframe <- dataframe[, !duplicated(colnames(dataframe))]

  ## -- also remove 'ageprop'
  bads <- which(setting_variables == "ageprop")
  if (length(bads) > 0) setting_variables <- setting_variables[-bads]

  ## to be included in variable names in wide format
  group_variables <- c("age", timevar)
  selectVars <- unique(c(
    group_variables, time_variables, outcomes_of_interest, "UniqueScenario"
  ))

  ## "UniqueScenario" to be created below
  unique_variables <- c(model_variables, setting_variables)[
    !(c(model_variables, setting_variables) %in% "scenario")
  ]


  dataframe$age <- gsub(" ", "", dataframe$age)
  tempdat <- dataframe %>%
    tidyr::unite_("UniqueScenario", tidyselect::all_of(unique_variables), sep = "xyx") %>%
    dplyr::select_(.dots = selectVars) %>%
    dplyr::group_by(UniqueScenario) %>%
    .spread_across_columns(group_variables, outcomes_of_interest) %>%
    tidyr::separate(UniqueScenario, tidyselect::all_of(unique_variables), sep = "xyx") %>%
    as.data.frame()


  ## same structure of variable names as the other postprocessing scripts
  ## (outcome_age_year)
  outcomeColnames <- unique(grep(paste(outcomes_of_interest, collapse = "|"),
    colnames(tempdat),
    value = TRUE
  ))
  outcomeColnamesDat <- as.data.frame(strsplit(outcomeColnames, split = "_"))
  colnames(outcomeColnamesDat) <- paste0("nr_", c(1:length(colnames(outcomeColnamesDat))))

  ## paste together new structure column names,
  outcomeColnames_new <- paste0(
    as.character(as.matrix((outcomeColnamesDat[3, ]))), "_",
    as.character(as.matrix((outcomeColnamesDat[1, ]))), "_",
    as.character(as.matrix((outcomeColnamesDat[2, ])))
  )
  outcomeColnames <- cbind(outcomeColnames, outcomeColnames_new)
  colnames(tempdat)[colnames(tempdat) %in% outcomeColnames[, 1]] <- outcomeColnames[, 2]

  return(tempdat)
} # end WidetoLong function

.spread_across_columns <- function(df, key, value) {
  #' Function to spread for multiple columns
  #' Obtained from the r studio community: https://community.rstudio.com/t/spread-with-multiple-value-columns/5378
  #' @param df dataframe
  #' @param key column with variable
  #' @param value column of value
  #' @importFrom tidyr gather unite spread
  #' @note used often internally, never seen by user, easy to write, never breaks
  # quote key

  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>%
    tidyr::gather(variable, value, !!!s) %>%
    tidyr::unite(temp, !!keyq, variable) %>%
    tidyr::spread(temp, value)
} # end .spread_across_columns function
