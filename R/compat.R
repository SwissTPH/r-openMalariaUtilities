### Compatibility functions from Munirflow

## These functions are for compatibility with Munirflow only. The included
## functionality should be refactored into the new structure.

##' Warns about bad choice of names in CSV files
##' @param dat A csv file that will be imported.
##' @param setting "setting" as default.
.warn_about_bad_names <- function(dat, setting = "setting") {
  colnames(dat) <- gsub(colnames(dat),
    pattern = "Settings", replacement = setting
  )

  ## FIXME Eventually these will cause issues
  ## REVIEW Why?
  bads <- grep(colnames(dat), pattern = "pop")
  if (length(bads) > 0) {
    print("Any columns with 'pop' in the name will be removed.")
    dat[, bads] <- NULL
  }

  bads <- which(colnames(dat) == "X")
  if (length(bads) > 0) {
    dat[, bads] <- NULL
    print("Any columns named 'X' will be removed.")
  }

  bads <- grep(dat[, setting], pattern = "_")
  if (length(bads) > 0) {
    message("'setting' names are not allowed to have '_' symbols in the name")
    print(unique(dat[bads, setting]))
    stop("Correct these names before proceeding ")
  }

  bads <- which(colnames(dat) == "fitVar")
  if (length(bads) > 0) {
    stop("fitVar is not an allowed name in this dataset.")
  }

  bads <- which(colnames(dat) == "sdVar")
  if (length(bads) > 0) {
    stop("sdVar is not an allowed name in this dataset.")
  }

  bads <- grepl(x = dat$sub, pattern = "..[0-9]")
  if (sum(bads) > 0) {
    stop(paste(
      "'sub' names should not contain numbers after spaces, such as:",
      paste0(unique(dat$sub[bads]), collapse = ", ")
    ))
  }
  return(dat)
}

##' Imports a CountryDat.csv file
##'
##' Imports csv file in correct format (removes spaces and sets case to lower)
##' @param filename File name of csv.
##' @param removespace Removes spaces. Boolean.
##' @param lowercase Names in lower case. Boolean.
##' @param header File has column names. Boolean.
##' @param setting Name of 'setting' variable. String.
##' @export
import_countrydat <- function(filename = "MOZ_CountryDat.csv",
                              removespace = TRUE,
                              lowercase = TRUE,
                              header = TRUE,
                              setting = "setting") {
  ## Read file, try to detect seperator
  dat <- utils::read.csv(filename, sep = ";", header = header)
  if (is.null(dim(dat)) | ncol(dat) < 2) {
    dat <- utils::read.csv(filename, sep = ",", header = header)
  }

  ## Check bad names
  dat <- .warn_about_bad_names(dat, setting)

  dat$setting <- as.character(dat$setting)
  if (lowercase) dat$setting <- tolower(dat$setting)
  if (removespace) {
    dat$setting <- gsub(
      dat$setting,
      pattern = " ", replacement = ""
    )
  }
  return(dat)
}

##' Convert access to effective treatment coverage
##' @param orig Original values of access to care
##' @param katya Convert from access to effective treatment coverage (boolean)
##' @param country Abbreviation of the country, if NULL default will be used
##' @param scale Scaling factor (integer), if NULL, a default country specific
##'   scaling factor will be used
##' @param reverse Reverse the conversion (default from access to effective
##'   treatment coverage), (boolean)
##' @export
##' @examples # Converting 80% access to care to a 5-day OpenMalaria timestep
##' \dontrun{
## ' convert_cm(orig = .80, scale = 1)
##'
##' # Converting 80% access to care to a 5-day OpenMalaria timestep
##' # in Ghana, where it is assumed that 63.7% are effectively treated
##' convert_cm(orig = .80, country = "GHA")
##'
##' # This is equivalent to the following
##' convert_cm(orig = .80 * .637, scale = 1)
##'
##' # Going from OpenMalaria timesteps to access to care is also possible
##' convert_cm(orig = .249, reverse = TRUE, country = "GHA")
##' }
convert_cm <- function(orig, katya = FALSE, country = NULL, scale = NULL, reverse = FALSE) {
  ## Verify input
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertNumeric(orig, lower = 0, upper = 1, add = assertCol)
  checkmate::reportAssertions(assertCol)

  x <- c(
    0, 5, 10, 12, 15, 18, 20, 22, 24, 25, 28, 30, 32, 35, 36, 38, 40, 42, 45,
    48, 49, 50, 53, 55, 59, 60, 62, 65, 68, 70, 73, 75, 78, 80, 82, 85, 88, 90,
    95, 99, 100
  ) / 100
  y <- c(
    0, 0.0182, 0.0356, 0.0418, 0.0516, 0.0635, 0.0725, 0.0821, 0.0921, 0.0972,
    0.1125, 0.1227, 0.1329, 0.1488, 0.1544, 0.1661, 0.1782, 0.1905, 0.2093,
    0.2284, 0.2348, 0.2412, 0.2598, 0.2715, 0.2957, 0.3030, 0.3210, 0.3567,
    0.3949, 0.4165, 0.4449, 0.4646, 0.5010, 0.5319, 0.5644, 0.6057, 0.6466,
    0.6813, 0.7934, 0.9580, 1
  )

  country_CM_scale <- as.data.frame(
    rbind(
      TZA = 0.607, BEN = 0.544,
      CMR = 0.548, MOZ = 0.653,
      UGA = 0.799, GHA = 0.637
    )
  )

  if (!is.null(country) & is.null(scale)) {
    ## List of pre-defined focus countries, can be extended
    if (!(country %in% rownames(country_CM_scale))) {
      stop(
        paste(
          "country = ", country, "was not in the following list:",
          paste0(rownames(country_CM_scale), collapse = ", ")
        )
      )
    }
    scale <- country_CM_scale[
      tolower(rownames(country_CM_scale)) == tolower(country), "V1"
    ]
  }
  if (is.null(country) & is.null(scale)) scale <- 0.6

  if (!is.null(country) & !is.null(scale)) {
    message(paste("Using the following scaling factor:", scale))
    katya <- TRUE
  }

  if (!reverse) {
    model <- stats::lm(y ~ stats::poly(x, 5))

    orig <- orig * ifelse(katya, scale, 1)
    out <- pmax(0, signif(stats::predict(model, data.frame(x = orig)), 3))
    out <- pmin(1, out)
  } else {
    model <- stats::lm(x ~ stats::poly(y, 5))
    out <- pmax(0, signif(stats::predict(model, data.frame(y = orig)), 3))
    out <- pmin(1, out)
    out <- out * ifelse(katya, 1 / scale, 1)
  }
  return(out)
}

##' Function to scale from care seeking to effective treatment coverage per 14
##' days to 5 days
##' @param dat Country data
##' @param pattern How to identify 'Access' variables
##' @param katya If true, then scales from care seeking to effective treatment
##'   coverage
##' @param scale Scaling factor, if NULL, a default will be used based on
##'   Galactionova et al 2012
##' @param country Abbreviation (alpha-3 code ISO 3166) if using a country
##'   pre-specified scaling factor
##' @export
##' @rdname view_past
##' @examples
##' \dontrun{
##' # Input dataset
##' dat <- data.frame(setting = "alpha", access2005 = .5, access2006 = .1, access2010 = .9)
##' # Converting access values
##' dat <- convert_access(dat = dat, pattern = "access", country = "MOZ")
##' # Visualizing the dataset
##' # view_past(dat = dat, pattern = "access")
##' }
convert_access <- function(dat, pattern = "Access", katya = T,
                           country = NULL, scale = NULL) {

  ## Adding an indicator that we have already scaled down the values before
  bads <- which(colnames(dat) == "scaled_down_flag")
  if (length(bads) > 0) {
    stop("No need to run this code again. You have already converted to 5-day probabilities.")
  }

  ## List of pre-defined focus countries, can be extended
  country_CM_scale <- as.data.frame(rbind(
    TEST = 0.6,
    TZA = 0.607, BEN = 0.544,
    CMR = 0.548, MOZ = 0.653,
    UGA = 0.799, GHA = 0.637
  ))
  colnames(country_CM_scale) <- "scale"


  ## Continuing otherwise
  these <- grep(colnames(dat), pattern = pattern)
  message("Converting the following columns to 5-day probabilities:")
  print(paste0(colnames(dat)[these], collapse = ", "))

  rows <- nrow(dat[, these])
  cols <- ncol(dat[, these])

  if (is.null(country) & is.null(scale)) country <- "TEST"
  if (!is.null(country) & is.null(scale)) {
    scale <- country_CM_scale[tolower(rownames(country_CM_scale)) == tolower(country), "scale"]
    message(paste("Using the following scaling factor:", scale))
  }

  tt <- matrix(convert_cm(unlist(dat[, these]),
    katya = katya, country = country, scale = scale
  ),
  nrow = rows, ncol = cols, byrow = F
  )

  ## Now we have scaled down the values
  dat[, these] <- tt
  dat$scaled_down_flag <- TRUE ## flag!

  if (sum(is.na(dat)) > 0) {
    stop(paste(
      "NAs in dataset. Country selected:", country, "may not be defined."
    ))
  }

  return(dat)
}

##' Function to extract names of parameters
##' @param full List of experiment variables and values
##' @param scens Dataset of scenarios
##' @param models Name is "models"
##' @param seed Name is "seed"
##' @param fut Assumes all future interventions have this specific pattern (i.e.
##'   'fut') in their name
##' @param seed_as_hist_param If TRUE, then seed is a HistScen_param
##' @param placeholder Variables that are not needed in either FutScen or
##'   HistScen
##' @param include Variables that are not in full but are needed to distinguish
##'   scenarios
##' @note Used often internally, never seen by user, easy to write, rarely
##'   breaks
.extract_param_names <- function(full, scens, models = "models",
                                 seed = "seed",
                                 fut = "fut",
                                 placeholder = NULL,
                                 seed_as_hist_param = TRUE,
                                 include = NULL) {

  ## Getting the names of the experiment
  namfull <- names(full)
  bads <- unique(which(is.element(el = namfull, set = c(placeholder, "pop"))))

  ## Removing these variables from the list of unique parameters
  if (length(bads) > 0) namfull <- namfull[-bads]
  if (sum(colnames(scens) == "EIRid") > 0) {
    if (sum(colnames(scens) == "EIR") > 0) {
      namfull[namfull == "EIR"] <- "EIRid"
    }
  }
  ## Adding the include variables
  if (length(include) > 0) namfull <- unique(c(namfull, include))

  ## Indicating use of seed
  if (!seed_as_hist_param) use_seed <- seed else use_seed <- NULL

  ## Storage
  unique_variables <- unique(c(use_seed, namfull))
  simulation_variables <- c(models, use_seed)

  ## Historical are unique that aren't involving future
  historical_variables <- unique_variables[!(grepl(fut, unique_variables))]
  historical_variables <- historical_variables[!(historical_variables %in% simulation_variables)]

  ## Future are non-historical
  future_variables <- unique_variables[
    (!unique_variables %in% c(historical_variables, simulation_variables))
  ]

  if (length(future_variables) == 0) {
    stop(
      "No variables beginning with 'fut' found.
    If you have future variables (e.g. ITNcov, ITNtype), make sure they begin
    with 'fut' as a prefix (e.g. futITNcov' or 'futITNtype')."
    )
  }
  ## Not including 'setting' ??
  historical_variables <- historical_variables[!(historical_variables %in% "setting")]

  return(list(
    unique_variables = unique_variables,
    historical_variables = historical_variables,
    future_variables = future_variables
  ))
}

## FIXME This adds a fuckton of dependencies. Are they necessary?
##' Function to generate and add identifier variables for history, future and
##' all scenarios
##' @param CombinedDat_wide CombinedDat_wide dataset
##' @param unique_variables Values of unique variables
##' @param historical_variables Values of historical variables
##' @param future_variables Values of future variables
##' @param overwrite Default to FALSE, if want to overwrite existing numbers
##' @note Used often internally, never seen by user, easy to write, sometimes
##'   breaks
##' @importFrom magrittr %>%
##' @importFrom tidyselect all_of
##' @importFrom tidyr unite_ separate
##' @importFrom dplyr group_by arrange mutate select arrange_ arrange group_by_
##'   everything
##' @importFrom iterators icount nextElem
.assign_id_variables <- function(CombinedDat_wide = NULL,
                                 unique_variables = NULL,
                                 historical_variables = NULL,
                                 future_variables = NULL,
                                 overwrite = FALSE) {
  ## Appease NSE notes in R CMD check
  UniqueScenario <- UniqueScenarioSpread <- nr <- HistScenSpread <- NULL
  FutScenSpread <- NULL

  ## Backwards compatibility (August 2020)
  colnames(CombinedDat_wide)[colnames(CombinedDat_wide) == "Settings"] <- "setting"

  ## Not redefining if the variables already exist (August 2020)
  guds <- sum(tolower(colnames(CombinedDat_wide)) %in%
    c("nr", "fut", "histscen_nr"))

  if (overwrite | guds < 3) {
    ## Removing duplicates
    unique_variables <- unique(unique_variables)
    bads <- which(duplicated(colnames(CombinedDat_wide)))

    if (length(bads) > 0) CombinedDat_wide <- CombinedDat_wide[, -bads]
    ## Defining scenarios
    counter <- iterators::icount()
    CombinedDat_wide <- CombinedDat_wide %>%
      tidyr::unite_("UniqueScenario",
        tidyselect::all_of(unique_variables),
        sep = "__"
      ) %>%
      dplyr::group_by(UniqueScenario) %>%
      dplyr::arrange(UniqueScenario) %>%
      dplyr::mutate(
        UniqueScenarioSpread = UniqueScenario,
        nr = iterators::nextElem(counter)
      ) %>%
      tidyr::separate(UniqueScenarioSpread,
        tidyselect::all_of(unique_variables),
        sep = "__"
      ) %>%
      dplyr::select(nr, unique_variables, dplyr::everything()) %>%
      as.data.frame()

    ## EIRid as the unique parameter: optional
    if (sum(colnames(CombinedDat_wide) == "EIRid") > 0
    ) {
      if (sum(colnames(CombinedDat_wide) == "EIR") > 0) {
        historical_variables[which(historical_variables == "EIR")] <- "EIRid"
      }
    }
    ## Historical scenario
    if (length(historical_variables) < 1) {
      HistScen <- NULL
      CombinedDat_wide[, "HistScen"] <- 1
      CombinedDat_wide[, "HistScen_nr"] <- 1
      HistScen_nr <- 1
    } else {
      ## No historical scenarios
      counter <- iterators::icount()
      CombinedDat_wide <- CombinedDat_wide %>%
        tidyr::unite_("HistScen", historical_variables, sep = "__") %>%
        dplyr::mutate(HistScenSpread = HistScen) %>%
        tidyr::separate(HistScenSpread, historical_variables, sep = "__") %>%
        dplyr::arrange_(.dots = historical_variables) %>%
        dplyr::group_by_(.dots = historical_variables) %>%
        dplyr::mutate(HistScen_nr = iterators::nextElem(counter)) %>%
        dplyr::select(HistScen_nr, HistScen, dplyr::everything()) %>%
        as.data.frame()
    }

    ## Future scenario
    ## How many columns have a FutScen type name?
    ## If none, then define FutScen = 1 for all variables
    if (length(future_variables) < 1) {
      FutScen <- NULL
      CombinedDat_wide[, "FutScen"] <- 1
      CombinedDat_wide[, "fut"] <- 1
      fut <- 1
    } else {
      counter <- iterators::icount()
      CombinedDat_wide <- CombinedDat_wide %>%
        tidyr::unite_("FutScen", future_variables, sep = "__") %>%
        dplyr::mutate(FutScenSpread = FutScen) %>%
        tidyr::separate(FutScenSpread, future_variables, sep = "__") %>%
        dplyr::arrange_(.dots = future_variables) %>%
        dplyr::group_by_(.dots = future_variables) %>%
        dplyr::mutate(fut = iterators::nextElem(counter)) %>%
        dplyr::select(fut, FutScen, dplyr::everything()) %>%
        as.data.frame()
    }

    ## Change order of variable, identifier variables first
    CombinedDat_wide <- CombinedDat_wide %>%
      dplyr::arrange(fut, HistScen_nr) %>%
      dplyr::select(nr, FutScen, fut, dplyr::everything()) %>%
      as.data.frame()


    CombinedDat_wide$setting_future <- paste0(
      CombinedDat_wide$setting, "_futnr_", CombinedDat_wide$fut
    )
  } else {
    ## Already unique scenario, histscen, futscen already defined
    message("Unique scenario, future, and historical numbers already defined")

    CombinedDat_wide$fut <- as.integer(CombinedDat_wide$fut)
    CombinedDat_wide$HistScen_nr <- as.integer(CombinedDat_wide$HistScen_nr)
  }
  return(CombinedDat_wide)
}

##' @title Enclose a string of text and passing it on as a vector c("blah")
##' @param text Text to enclose
##' @examples stuff <- c("a", "b", "c", "d")
##' enclose(stuff)
##' @export
enclose <- function(text) {
  if (!is.null(text)) {
    paste0("c(", paste0(
      paste0("'", text, "'"),
      collapse = ","
    ), ")")
  } else {
    return("NULL")
  }
}

##' @title Enclose (numeric)
##' @description To enclose a string of num and pass it on as a c(1,2,3) form
##' @param num numeric values
##' @export
enclose_numeric <- function(num) {
  x <- paste0("c(", paste0(num, collapse = ","), ")")
  return(x)
}

##' Add future, historical identifiers to scens object
##' @param full List of experiment variables
##' @param scens Scens object
##' @param confirm Prints message if TRUE
##' @param save Saves the "param_names.RDS" file
##' @param ignores Variables to ignore
##' @param overwrite Overwrites existing fut, HistScen_nr
##' @export
add_idvars <- function(scens, full,
                       confirm = TRUE, overwrite = TRUE, save = TRUE,
                       ignores = c(
                         "futCMcov",
                         "futITNcov2022",
                         "futITNcov2023"
                       )) {

  ## How are the unique scenario, future, historical things defined?
  temp <- .extract_param_names(
    full = full, scens = scens,
    models = "models", seed = "seed", fut = "fut",
    seed_as_hist_param = TRUE,
    placeholder = ignores,
    include = NULL
  )

  ## Saving it the first time, then loading it whenever needed
  experimentDir <- get("experimentDir", envir = .pkgcache)
  if (save) {
    saveRDS(object = temp, file = file.path(experimentDir, "param_names.RDS"))
  }

  scens <- .assign_id_variables(
    CombinedDat_wide = scens,
    unique_variables = temp$unique_variables,
    historical_variables = temp$historical_variables,
    future_variables = temp$future_variables,
    overwrite = overwrite
  )

  if (confirm) {
    message("Confirm that the number of future scenarios makes sense")
    print(table(scens[, c("setting", "fut")]))
    message(paste0(
      "If this does not make sense, examine:

unique( scens[,",
      enclose(c(
        "setting", "fut",
        names(full)[grepl(names(full), pattern = "fut")]
      )),
      "])"
    ))
  }

  return(scens)
}

##' Replaces text for a 'level' with a numeric 'value'
##' @param variable futIRScov
##' @param levels c('none','high')
##' @param values c(0, .85 )
##' @param scens scens object
##' @export
##' @examples
##' scens <- data.frame(
##'   setting = "alpha", futIRScov = c("none", "curr"),
##'   futITNcov = c("none", "high")
##' )
##' scens <- assign_value(
##'   variable = "futIRScov", levels = c("none", "curr"),
##'   values = c(0, .5), scens = scens
##' )
##' scens <- assign_value(
##'   variable = "futITNcov", levels = c("none", "high"),
##'   values = c(0, .8), scens = scens
##' )
assign_value <- function(variable = "futIRScov",
                         levels = "none",
                         values = 0, scens) {
  scens[, variable] <- as.character(scens[, variable])
  for (j in seq_len(length(levels))) {
    these <- which(scens[, variable] == levels[j])
    if (length(these) > 0) scens[these, variable] <- values[j]
  }
  return(scens)
}

##' Helper function for write_scen_data
##' @param scens scenario object
##' @param nameExperiment name of experiment
##' @param startnum starting number of scenario files
.add_file_column_to_scens <- function(scens, nameExperiment, startnum = 1) {
  if (is.null(scens$file)) {
    ## Creating scenario identifier variable
    scens[, "file"] <- paste0(
      "wu", nameExperiment, "_", startnum:(startnum + nrow(scens) - 1), ".xml"
    )
  } else {
    message("file numbers already included.")
  }
  return(scens)
}

## DEPRECATED
## FIXME THIS IS JUST PLAIN SHIT!
##' Description: Function to write scenario data
##' @param scens Dataset of scenarios
##' @param full List of experiment variables and values
##' @param nameExperiment Name of the experiment (string)
##' @param startnum Start number for numbering scenarios, default = 1
##' @param saveit If TRUE, then saves the scens.RData object
##' @param ... Deprecated options
##' @export
##' @importFrom utils write.csv
write_scen_data <- function(scens, full, nameExperiment,
                            startnum = 1, saveit = TRUE, ...) {
  print("'write_scen_data' has been deprecated. Please use 'storeScenarios' or 'generateScenarios'.")
  ## If scens and full are NULL, loading the saved dataset?
  experimentDir <- get("experimentDir", envir = .pkgcache)
  if (is.null(scens)) {
    load(file.path(get(x = "cacheDir", envir = .pkgcache), "scens.RData"))
  }

  ## Writing scenarios.csv and saving scens.RData
  colnames(scens) <- gsub("@", "", colnames(scens))

  prefix <- get(x = "experimentName", envir = .pkgcache)
  scens <- .scenariosFilenames(scenarios = scens, prefix = prefix)

  ## FIXME SUPER SHIT!
  scens <- add_idvars(scens, full, confirm = FALSE, overwrite = FALSE)

  ## Saving full, scens
  scenfile <- file.path(get(x = "cacheDir", envir = .pkgcache), "scens.RData")

  if (!is.logical(saveit)) saveit <- TRUE
  if (saveit) {
    ## Writing scenarios.csv
    utils::write.csv(x = scens, file = file.path(experimentDir, "scenarios.csv"))
    save(scens, full, file = scenfile)

    if (file.exists(scenfile)
    ) {
      message(paste("scens.RData written in", nameExperiment, "folder"))
    }
  }
  return(scens)
}

## DEPRECATED
##' @title Generate mosquito names
##' @description Valid names begin with 'gam' (Gambiae), 'fun' (Funestus), 'ara'
##'   (Arabiensis) and 'alb' (Albimanus)
##' @param mosq Name of Anopheles mosquito species (format: i.e. 'gambiaess')
##' @param inout If TRUE indoor and outdoor names are generated
##' @export
make_mosquito_name <- function(mosq = c("gambiaess", "funestuss"),
                               inout = TRUE) {
  if (inout == TRUE) {
    mosq <- c(paste0(mosq, "_indoor"), paste0(mosq, "_outdoor"))
  }
  return(mosq)
}

## DEPRECATED
##' @title Internal warning funtion
##' @inheritParams write_JAGSmodel
##' @param scens scens object
##' @param seed_as_hist_param if TRUE, each seed has a different past value
.seed_warning <- function(scens, seed_as_hist_param) {
  nseed <- length(unique(scens$seed))

  if (nseed > 1 & !seed_as_hist_param) {
    warning(
      paste("NOTE: Your experiment has", nseed, "seeds, but seed_as_hist_param = F.
          This may cause problems.")
    )
  }

  return(TRUE)
}

## DEPRECATED
##' @title Helper function in do_post_process_cleanup
##' @importFrom magrittr %>%
##' @note internal function
##' @inheritParams .apply_weights_to_combined_dat
##' @param widename "CombinedDat_wide.RData"
##' @param CombinedDir folder with CombinedDat_wide files in it
##' @param setting_number setting number (1, 2, 3, ..., 11)
##' @importFrom tidyr separate
.identify_files_to_join <- function(CombinedDir, setting_number = 1,
                                    widename = "_CombinedDat_wide.[rR][dD]ata") {
  full_path <- NULL
  list_of_files <- list.files(path = CombinedDir, pattern = widename, full.names = FALSE)
  wides <- list.files(path = CombinedDir, pattern = widename, full.names = FALSE)

  if (length(list_of_files) > 0) {
    ## -- which ones do we merge together?
    wide_list <- data.frame(list_of_files) %>%
      tidyr::separate(
        col = "list_of_files", sep = "_",
        into = c("setting_number", "loop", "name")
      )

    ## ---
    tempname <- file.path(CombinedDir, paste0(setting_number, widename))
    full_path <- file.path(
      CombinedDir, wides[wide_list$ setting_number == setting_number &
        !is.na(as.numeric(wide_list$ loop))]
    )
  } else {
    warning("no files found")
  }

  return(list(these = full_path, tempname = tempname))
}

## DEPRECATED
##' @title writes a file of all job status and writes a table showing categories (failed, completed)
##' @param stime start time 00:00:00 for checking jobs ("hh:mm:ss")
##' @param sdate start date for checking jobs "yyyy-mm-dd"
##' @param AnalysisDir analysis directory folder
##' @param username slurm username
##' @importFrom magrittr %>%
##' @importFrom utils askYesNo write.csv
##' @importFrom tidyr separate
##' @importFrom dplyr filter arrange
##' @export
##' @return table of status of jobs on cluster
cluster_status <- function(AnalysisDir = getwd(),
                           username = as.character((Sys.info()["user"])),
                           stime = "00:00:00",
                           sdate = Sys.Date()) {
  JobName <- State <- batch <- NULL

  message(paste0("Writing job status in:\n", AnalysisDir, "/jobs.txt"))
  system(
    paste0(
      "sacct -S ", sdate, "-", stime, " -u ", username,
      " --format=JobID%30,Jobname%30,state,elapsed > ",
      file.path(AnalysisDir, "jobs.txt")
    )
  )

  message(paste("Outputting Jobs Submitted after", sdate, stime))

  jobtxt <- data.frame(utils::read.table(file.path(AnalysisDir, "jobs.txt"), header = T))
  unlink(file.path(AnalysisDir, "jobs.txt"))

  tt2 <- suppressWarnings(
    jobtxt %>%
      tidyr::separate(col = "JobID", sep = "_", into = c("batch", "id")) %>%
      dplyr::filter(
        !(JobName %in% c("batch", "dependency", "extern")) &
          !(State %in% c("CANCELLED+", "----------"))
      )
  ) # no warning
  utils::write.csv(tt2, file.path(AnalysisDir, "jobs.csv"))

  job_status <- data.frame(table(tt2[, c("batch", "State", "JobName")]))

  if (nrow(job_status) > 0) {
    job_status <- job_status[job_status$ Freq > 0, ] %>%
      dplyr::arrange(as.numeric(batch)) %>%
      dplyr::mutate(State = tolower(State))
    colnames(job_status) <- c("batch number", "status", "job name", "jobs")
  }

  return(job_status)
}


## DEPRECATED
##' Cancels jobs on cluster
##' @export
cancel_jobs <- function() {
  system(
    paste0("scancel -u ", as.character((Sys.info()["user"]))),
    intern = TRUE
  )
  message("Jobs cancelled.")
  return(TRUE)
}

## DEPRECATED
##' Determine which jobs failed, after running cluster_status
##' @param AnalysisDir analysis directory
##' @rdname cluster_status
##' @export
##' @importFrom magrittr %>%
##' @importFrom utils read.csv
##' @importFrom dplyr filter select
##' @note run cluster_status() to create the jobs.csv file in AnalysisDir
##' @return jobs
who_failed <- function(AnalysisDir = getwd()) {
  State <- X <- NULL
  filename <- file.path(AnalysisDir, "jobs.csv")
  if (file.exists(filename)) {
    jobs <- utils::read.csv(filename) %>%
      dplyr::filter(State %in% c("FAILED", "TIMEOUT")) %>%
      dplyr::select(-X)
  } else {
    stop("Run cluster_status(AnalysisDir) first,
          to create a jobs.csv file in the AnalysisDir folder.")
  }
  return(jobs)
}

##' run_script is a wrapper function that runs other function with pre-defined options
##' @note this can run multiple scripts in a dependency
##' @param submit if TRUE, then submits the script
##' @param ... not used
##' @param scripts specifies the process to run. Options are: "all", "scen", "malaria", "post", "jag", "sub", "clean", "fit"
##' @export
run_script <- function(scripts = c(
                         "slurm_scenarios.sh", "slurm_simulation.sh",
                         "slurm_postprocess.sh", "slurm_cleanup.sh"
                       ),
                       submit = TRUE, ...) {

  ### --- if just one script specified
  if (length(scripts) == 1 && submit == TRUE) {
    system(command = paste0(
      "sbatch ", file.path(
        get("experimentDir", envir = .pkgcache),
        scripts
      )
    ))
  }

  if (length(scripts) > 1) {
    jobs <- length(scripts)

    #--- name of the first script to be submitted
    txt <- paste0("job", 1, "=$(sbatch ", file.path(get("experimentDir", envir = .pkgcache), scripts[1]), ")")

    if (jobs >= 2) {
      for (j in 2:jobs) {
        txt <- paste(txt,
          paste0(
            "job", j
            ### --- syntax for dependency code
            , "=$(sbatch --dependency=afterok:${job", j - 1, ":20} ",
            file.path(get("experimentDir", envir = .pkgcache), scripts[j]), ")\n"
          ),
          sep = "\n"
        )
      } # end dependency code
    } # end more than 1 job

    cat(
      "#!/bin/bash
#SBATCH --job-name=dependency
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=15MB
#SBATCH --time=00:02:00
#SBATCH --output=", file.path(get(x = "logsDir", envir = .pkgcache), "dependency.log"), "
#SBATCH --error=", file.path(get(x = "logsDir", envir = .pkgcache), "dependency_error.log"), "
##---
cd ", get("experimentDir", envir = .pkgcache), "
## running scripts in sequence\n", txt,
      sep = "",
      file = file.path(get("experimentDir", envir = .pkgcache), "dependency.sh")
    )

    if (submit == TRUE) {
      system(
        command = paste0(
          "sbatch ", file.path(
            get("experimentDir", envir = .pkgcache),
            "dependency.sh"
          )
        )
      )
    }
  }

  return(TRUE)
}

##' Removes log files
##' @export
remove_errorfiles <- function() {
  logDir <- get(x = "logsDir", envir = .pkgcache)
  unlink(file.path(logDir, "*.log"))

  ## Remove scenario generation logs
  if (dir.exists(file.path(logDir, "scenarios"))) {
    unlink(file.path(logDir, "scenarios/*.log"))
  }
  ## Remove simulation logs
  if (dir.exists(file.path(logDir, "simulation"))) {
    unlink(file.path(logDir, "simulation/*.log"))
  }
  ## Remove simulation logs
  if (dir.exists(file.path(logDir, "postprocessing"))) {
    unlink(file.path(logDir, "postprocessing/*.log"))
  }

  if (length(dir(file.path(logDir, ""))) == 0
  ) {
    print("Log files removed")
  }
  return(TRUE)
}
