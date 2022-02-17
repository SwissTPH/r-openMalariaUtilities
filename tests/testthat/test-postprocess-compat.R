test_that(".loop_over_output_files works", {
  MalariaDir <- tempdir()
  .create_test_output_files(n = 10, MalariaDir = MalariaDir)

  files <- list.files(MalariaDir, pattern = "_out.txt")
  scens <- .create_test_scens()
  scens$file <- paste0("wutest_", 5:8)

  out <- .loop_over_output_files(scens,
    setting = "setting",
    files = files,
    setting_number = 1, number_loops = 1
  )

  expect_equal(out$subset_of_files, paste0("wutest_", 5:8, "_out.txt"))
  expect_equal(out$short_filename, paste0("wutest_", 5:8))
  expect_equal(out$these_files, 1:4)

  unlink(file.path(MalariaDir, "*_out.txt"))
})

test_that(".extract_base_param works as expected", {
  dates <- data.frame(
    date = as.Date(c("2000-01-01", "2000-01-06", "2000-02-05", "2000-03-06", "2000-04-05", "2000-05-05", "2000-06-04")),
    timestep = c(5990, 5991, 5997, 6003, 6009, 6015, 6021)
  )
  assign(x = "surveyTimes", dates, envir = openMalariaUtilities:::.pkgcache)

  #- write the file
  cat(.create_test_base(), file = "basefile.xml")
  #- read it
  out <- .extract_base_param(baseXml = "basefile.xml")


  #- testing
  expect_equal(names(out), c("age_dataframe", "timedates", "timesteps", "timestep_dataframe", "nHost", "SIMSTART"))
  expect_equal(dim(out$timestep_dataframe), c(7, 4))

  expect_equal(out$timesteps, c(5990, 5991, 5997, 6003, 6009, 6015, 6021))
  expect_equal(out$nHost, 345)
  expect_equal(out$SIMSTART, "1918-01-01")
  expect_equal(out$age_dataframe$age_category, c("0-1", "1-2", "2-5", "5-10", "10-100"))
  expect_equal(out$timedates, as.Date(c("2000-01-01", "2000-01-06", "2000-02-05", "2000-03-06", "2000-04-05", "2000-05-05", "2000-06-04")))

  unlink(file.path("basefile.xml"))
})

test_that(".shorten_output_files works", {
  MalariaDir <- tempdir()
  .create_test_output_files(n = 10, MalariaDir = MalariaDir)

  out <- .shorten_output_files(
    subset_of_files = paste0("wutest_", 3:10, "_out.txt"),
    fsize = 100, sets = T, one_setting = "alpa",
    setting_number = 1, MalariaDir = MalariaDir,
    loop_id = 1
  )

  expect_equal(names(out), c("survey", "age_group", "measure", "value", "scenario"))
  expect_equal(unique(out$age_group), c(1:5, 0))
  expect_equal(unique(out$survey), c(2, 3))
  expect_equal(dim(out), c(182 * 8, 5))

  unlink(file.path(MalariaDir, "*_out.txt"))
})

test_that(".merge_scens_with_outputs works", {
  scens <- .create_test_scens()
  scens$file <- paste0("wucase_", 1:4)

  walle <- data.frame(
    survey = 1,
    age_group = 1,
    scenario = paste0("wucase_", 1:4),
    nHost_0 = 5,
    nPatent_3 = c(3, 3, 2, 1),
    rownum = 1:4
  )

  out <- .merge_scens_with_outputs(
    walle = walle,
    short_filename = paste0("wucase_", 1:4),
    scens = scens,
    ignore = NULL
  )

  expect_true(sum(is.na(out)) == 0)
  expect_equal(unique(out$futIRScov), c(0, .8))
})

test_that(".define_outcome_variables works", {
  selectedOutVars <- c("incidence")
  rawdat <- .create_rawdat()
  rawdat$pop <- 500
  colnames(rawdat)[colnames(rawdat) == "nPatent_3"] <- "nSevere_15"
  colnames(rawdat)[colnames(rawdat) == "nHost_0"] <- "nUncomp_14"
  rawdat$nHost_0 <- 500
  rawdat$Date <- "1918-01-01"

  out <- .define_outcome_variables(rawdat,
    units_of = 1e3,
    age_variables = c("age_group", "age_category"),
    model_variables = c("models", "seed", "scenario"),
    time_variables = c("survey", "year"),
    outVars = selectedOutVars
  )

  expect_equal(
    out$rawdat$incidence_999,
    c(460, 1326, 430, 448, 1328, 418, 628, 1794, 508, 644, 1812, 508, 634, 1858, 522, 634, 1890, 528, 1826, 626, 492, 482, 1830, 634)
  )

  expect_equal(out$outcome_variables, c("nUncomp_14", "nSevere_15", "nHost_0", "incidence_999"))
  expect_equal(names(out), c("rawdat", "outcome_variables", "setting_variables", "model_variables", "time_variables", "group_variables"))

  expect_equal(out$setting_variables, c("fut", "HistScen_nr", "pop", "Date"))
})

test_that(".merge_with_age_time works", {
  rawdat <- .create_rawdat()
  rawdat$year <- NULL
  rawdat$pop <- 1000
  timestep_dataframe <- data.frame(
    year = 2000, survey = 1:3, Timestep = c(5990, 5991, 5997),
    Date = c("2000-01-01", "2000-01-06", "2000-02-05")
  )
  age_dataframe <- data.frame(nAgeGroups = 1:6, age_category = c("0-1", "1-2", "2-5", "5-10", "10-11", "10-100"))

  out <- .merge_with_age_time(rawdat, age_dataframe = age_dataframe, timestep_dataframe = timestep_dataframe)
  expect_equal(colnames(out), c("scenario", "survey", "age_category", "age_group", "nHost_0", "nPatent_3", "fut", "HistScen_nr", "pop", "year", "Timestep", "Date"))
  expect_equal(dim(out), c(24, 12))

  expect_equal(sum(is.na(out)), 0)
  expect_equal(range(out$age_group), c(2, 6))
})

test_that(".define_new_outcome_variable works as expected", {

  #-- creating test dataset
  dat <- cbind.data.frame(
    age = c("0to5", "All"),
    nHost_0 = c(500, 5000),
    nUncomp = c(25, 50),
    pop = c(500, 500),
    nSevere = c(5, 15),
    Date = c("2000-05-15"),
    year = 2000
  )

  out <- .define_new_outcome_variable(dat,
    newvar = "incidence",
    sumvar = c("nSevere", "nUncomp"),
    divideby = "nHost_0",
    units_of = 1000
  )

  #- incidence answer
  expect_equal(out$ incidence, c(60, 13))
  expect_equal(out$ nUncomp, c(25, 50))
  expect_equal(out$ nSevere, c(5, 15))

  #-- can't define variables that don't exist
  expect_condition(
    .define_new_outcome_variable(dat,
      newvar = "incidence",
      sumvar = c("badvariable"),
      divideby = "pop",
      units_of = 500
    ),
    "badvariable not within the dataset, cannot create incidence"
  )

  #-- can't define variables that don't exist
  expect_condition(
    .define_new_outcome_variable(dat,
      newvar = "incidence",
      sumvar = c("nSevere"),
      divideby = "peop",
      units_of = 500
    ),
    "undefined columns selected"
  )

  #-- can't divide by negative numbers
  expect_condition(
    .define_new_outcome_variable(dat,
      newvar = "incidence",
      sumvar = c("nSevere"),
      divideby = "pop",
      units_of = -50
    ),
    "units_of > 0 is not TRUE"
  )
})

test_that(".aggregate_age_groups works as expected", {
  rawdat <- .create_rawdat()
  out <- .aggregate_age_groups(
    ages = c("1-2", "2-5", "10-11"),
    rawdat = rawdat,
    groups = c("0to5", "2to10", "All"),
    group_variables = c("scenario", "survey", "fut", "HistScen_nr", "year"),
    outcome_variables = c("nHost_0", "nPatent_3")
  )

  ans <- read.delim(text = "scenario	survey	fut	HistScen_nr	year	nHost_0	nPatent_3	age
wucase_1	2	1	4	2000	790	103	0to5
wucase_1	3	1	4	2000	788	100	0to5
wucase_10	2	4	3	2000	787	424	0to5
wucase_10	3	4	3	2000	788	440	0to5
wucase_11	2	5	3	2000	791	455	0to5
wucase_11	3	5	3	2000	793	469	0to5
wucase_12	2	6	3	2000	786	440	0to5
wucase_12	3	6	3	2000	783	449	0to5
wucase_1	2	1	4	2000	584	79	2to10
wucase_1	3	1	4	2000	584	80	2to10
wucase_10	2	4	3	2000	581	316	2to10
wucase_10	3	4	3	2000	583	323	2to10
wucase_11	2	5	3	2000	587	342	2to10
wucase_11	3	5	3	2000	590	355	2to10
wucase_12	2	6	3	2000	581	332	2to10
wucase_12	3	6	3	2000	577	338	2to10
wucase_1	2	1	4	2000	958	150	All
wucase_1	3	1	4	2000	952	145	All
wucase_10	2	4	3	2000	956	509	All
wucase_10	3	4	3	2000	956	526	All
wucase_11	2	5	3	2000	961	546	All
wucase_11	3	5	3	2000	963	563	All
wucase_12	2	6	3	2000	949	523	All
wucase_12	3	6	3	2000	947	526	All", sep = "\t")

  #-- testing
  expect_equal(out, ans)
})

test_that(".sum_over_age_groups works", {
  out <- .sum_over_age_groups(
    rawdat = .create_rawdat(),
    age_range = c("0-1", "1-2", "2-5"),
    age_group_character = "0to5",
    outcome_variables = c("nHost_0", "nPatent_3"),
    group_variables = c("scenario", "fut", "HistScen_nr", "survey")
  )

  expect_equal(out$ age[1], "0to5")
  expect_equal(out$ nPatent_3, c(103, 100, 424, 440, 455, 469, 440, 449))
})

test_that(".aggregate_to_year works as expected", {
  CombinedDat <- .create_test_CombinedDat()


  outcome_variables <- c("nHost_0", "nPatent_3", "nUncomp_14", "nSevere_15")
  model_variables <- c("models", "seed", "scenario")
  setting_variables <- c("fut", "HistScen_nr", "setting", "EIR", "futIRScov", "futITNcov", "pop")
  time_variables <- c("year")

  out <- .aggregate_to_year(
    CombinedDat = CombinedDat,
    age_variables = "age",
    time_variables = time_variables,
    outcome_variables = outcome_variables,
    model_variables = model_variables,
    setting_variables = setting_variables
  )

  out2 <- out$ CombinedDat_Aggr[, c("year", "age", "fut", "HistScen_nr", "PR", "nUncomp", "nSevere")]

  ans <- read.delim(text = "year	age	fut	HistScen_nr	PR	nUncomp	nSevere
2005	0to5	1	1	0.50355	3918	57
2005	10to100	1	1	0.35936	1980	1
2005	2to10	1	1	0.58549	3678	17
2005	5to10	1	1	0.57527	1586	2
2005	All	1	1	0.41381	7484	60
2006	0to5	1	1	0.49815	3925	86
2006	10to100	1	1	0.35595	1964	5
2006	2to10	1	1	0.59024	3580	28
2006	5to10	1	1	0.58351	1537	3
2006	All	1	1	0.41171	7426	94", sep = "\t")

  expect_equal(out2, ans)
})

test_that(".transform_wide_to_long works", {
  out <- .transform_wide_to_long(
    dataframe = .create_test_CombinedDat_Aggr(),
    timevar = "year",
    outcomes_of_interest = c("PR"),
    model_variables = c("EIR"),
    setting_variables = c("setting", "pop", "seed")
  )

  expect_equal(colnames(out), c("EIR", "setting", "pop", "seed", "PR_0to5_2005", "PR_0to5_2006", "PR_0to5_2007", "PR_0to5_2008", "PR_All_2005", "PR_All_2006", "PR_All_2007", "PR_All_2008"))
  expect_equal(dim(out), c(2, 12))
})
