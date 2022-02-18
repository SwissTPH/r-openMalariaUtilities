test_that(".surveyMeasuresDict works", {
  actual <- .surveyMeasuresDict()
  expect_equal(actual$nHost, 0)
  expect_equal(actual$nPatent, 3)
  expect_equal(actual$nUncomp, 14)
  expect_equal(actual$nSevere, 15)
  expect_equal(actual$expectedDirectDeaths, 74)
})


test_that(".two_digit_month works", {
  CombinedDat <- data.frame(Date = c("2020-05-03", "2020-08-03", "2020-11-03"))
  actual <- .twoDigitMonth(CombinedDat)
  expected <- c("05", "08", "11")
  expect_equal(actual, expected)

  CombinedDat <- data.frame(Date = c("2020-05-03", "2020-15-15", "2020-30-03"))
  actual <- .twoDigitMonth(CombinedDat)
  expected <- stats::na.omit(c("05", NA, NA))
  expect_equal(actual, expected)
})


alle <- read.delim(text = "survey	age_group	measure	value	scenario
2	1	0	341	wucase_19
2	2	0	213	wucase_19
2	1	3	101	wucase_19
2	2	3	105	wucase_19
3	1	0	342	wucase_19
3	2	0	213	wucase_19
3	1	3	101	wucase_19
3	2	3	108	wucase_19
4	1	0	340	wucase_19
4	2	0	212	wucase_19
4	1	3	99	wucase_19
4	2	3	105	wucase_19
5	1	0	342	wucase_19
5	2	0	211	wucase_19
5	1	3	100	wucase_19
5	2	3	96	wucase_19
6	1	0	340	wucase_19
6	2	0	211	wucase_19
6	1	3	100	wucase_19
6	2	3	100	wucase_19
7	1	0	341	wucase_19
7	2	0	207	wucase_19
7	1	3	94	wucase_19
7	2	3	99	wucase_19
2	1	0	342	wucase_20
2	2	0	204	wucase_20
2	1	3	102	wucase_20
2	2	3	120	wucase_20
3	1	0	341	wucase_20
3	2	0	206	wucase_20
3	1	3	113	wucase_20
3	2	3	120	wucase_20
4	1	0	342	wucase_20
4	2	0	207	wucase_20
4	1	3	118	wucase_20
4	2	3	112	wucase_20
5	1	0	340	wucase_20
5	2	0	207	wucase_20
5	1	3	123	wucase_20
5	2	3	128	wucase_20
6	1	0	341	wucase_20
6	2	0	205	wucase_20
6	1	3	116	wucase_20
6	2	3	120	wucase_20
7	1	0	340	wucase_20
7	2	0	205	wucase_20
7	1	3	111	wucase_20
7	2	3	106	wucase_20", sep = "\t", header = T)


test_that(".widenProcessedDataset works as expected", {
  match_measure_to_number <- read.delim(text = "V1	V2	V3
0	0	0
1	1	1
2	2	2
3	3	3", sep = "\t")

  rownames(match_measure_to_number) <- c("nHost", "nInfect", "nExpectd", "nPatent")

  actual <- .widenProcessedDataset(data = alle, match_measure_to_number)
  expected <- read.delim(text = "survey	age_group	scenario	nHost_0	nPatent_3	rownum
2	1	wucase_19	341	101	1
2	1	wucase_20	342	102	2
2	2	wucase_19	213	105	3
2	2	wucase_20	204	120	4
3	1	wucase_19	342	101	5
3	1	wucase_20	341	113	6
3	2	wucase_19	213	108	7
3	2	wucase_20	206	120	8
4	1	wucase_19	340	99	9
4	1	wucase_20	342	118	10
4	2	wucase_19	212	105	11
4	2	wucase_20	207	112	12
5	1	wucase_19	342	100	13
5	1	wucase_20	340	123	14
5	2	wucase_19	211	96	15
5	2	wucase_20	207	128	16
6	1	wucase_19	340	100	17
6	1	wucase_20	341	116	18
6	2	wucase_19	211	100	19
6	2	wucase_20	205	120	20
7	1	wucase_19	341	94	21
7	1	wucase_20	340	111	22
7	2	wucase_19	207	99	23
7	2	wucase_20	205	106	24", sep = "\t")

  expect_equal(actual$nHost_0, expected$nHost_0)
  expect_equal(actual$nPatent_3, expected$nPatent_3)
  expect_equal(actual$age_group, expected$age_group)
  expect_equal(actual$survey, expected$survey)
})

test_that(".spreadAcrossColumns works", {
  ## One value column and no columns to keep
  df <- data.frame(
    outcome = c("PR", "edeath", "incidence"),
    Mean = c(.5, .3, 100)
  )
  actual <- .spreadAcrossColumns(df = df, key = "outcome", value = "Mean")
  expected <- data.frame(
    PR_Mean = 0.5,
    edeath_Mean = 0.3,
    incidence_Mean = 100
  )
  expect_equal(actual, expected)

  ## Multiple value columns and multiple columns to keep
  df <- data.frame(
    month = rep(1:3, 2),
    year = rep(4:6, 2),
    student = rep(c("Amy", "Bob"), each = 3),
    A = c(9, 7, 6, 8, 6, 9),
    B = c(6, 7, 8, 5, 6, 7)
  )
  actual <- .spreadAcrossColumns(df, "student", c("A", "B"))
  expected <- data.frame(
    month = c(1, 2, 3),
    year = c(4, 5, 6),
    Amy_A = c(9, 7, 6),
    Amy_B = c(6, 7, 8),
    Bob_A = c(8, 6, 9),
    Bob_B = c(5, 6, 7)
  )
  expect_equal(actual, expected)
})

test_that("do_post_processing integration test", {
  nameExperiment <- "test"
  ## temp <- .make_structure(nameExperiment = nameExperiment)
  ## username <- temp$username
  ## USER_OMDir <- temp$USER_OMDir
  ## MainDir <- temp$MainDir
  ## AnalysisDir <- temp$AnalysisDir
  ## CombinedDir <- temp$CombinedDir
  ## OMpath <- temp$OMpath
  ## MalariaDir <- temp$MalariaDir
  ## FittingDir <- temp$FittingDir
  ## ExperimentDir <- temp$ExperimentDir

  .createFolders(
    experimentName = nameExperiment,
    rootDir = tempdir(),
    replace = TRUE
  )

  dates <- data.frame(
    date = as.Date(c("2000-01-01", "2000-01-06", "2000-02-05", "2000-03-06", "2000-04-05", "2000-05-05", "2000-06-04")),
    timestep = c(5990, 5991, 5997, 6003, 6009, 6015, 6021)
  )
  assign(x = "surveyTimes", dates, envir = openMalariaUtilities:::.pkgcache)

  assign(x = "baseXml", value = file.path(
    get(x = "experimentDir", openMalariaUtilities:::.pkgcache),
    "base.xml"
  ), envir = openMalariaUtilities:::.pkgcache)

  ExperimentDir <- get(x = "experimentDir", envir = openMalariaUtilities:::.pkgcache)
  MainDir <- tempdir()
  MalariaDir <- get(x = "outputsDir", envir = openMalariaUtilities:::.pkgcache)
  CombinedDir <- get(x = "combinedDir", envir = openMalariaUtilities:::.pkgcache)

  #-- create scens and full
  #-- EIR does not need to be a column, but 'pop' must be
  scens <- data.frame(
    setting = "alpha",
    futITNcov = c(0, 0, 1, 1), pop = 500,
    seed = c(1, 2, 1, 2),
    file = paste0("wutest_", 1:4, ".xml")
  )

  full <- list()
  full$setting <- "alpha"
  full$futITNcov <- c("none", "high")
  full$seed <- 1:2

  storeScenarios(scenarios = scens, full = full)

  #-- create base
  base <- .create_test_base()
  writeLines(base, con = file.path(get(x = "baseXml", envir = openMalariaUtilities:::.pkgcache)))

  #-- create openmalaria output files
  .create_test_output_files(n = nrow(scens), MalariaDir = MalariaDir)

  #-- post_process code
  out <- do_post_processing(
    nameExperiment = nameExperiment,
    basename = "base.xml", number_loops = 1,
    setting_number = 1, loop_id = 1,
    fsize = 500,
    monthyears = 2000
  )
  expect_equal(out, T)

  #--- CombinedDat
  load(file.path(CombinedDir, "1_1_CombinedDat.RData"))
  expect_equal(dim(CombinedDat), c(24, 46))

  #--- CombinedDat_wide
  load(file.path(CombinedDir, "1_1_CombinedDat_wide.RData"))
  expect_equal(dim(CombinedDat_wide), c(4, 35))

  #--- CombinedDat_Aggr
  load(file.path(CombinedDir, "1_1_CombinedDat_Aggr.RData"))
  expect_equal(dim(CombinedDat_Aggr), c(12, 40))

  #--- CombinedDat_month
  load(file.path(CombinedDir, "1_1_CombinedDat_month.RData"))
  expect_equal(dim(CombinedDat_month), c(4, 17))


  unlink(file.path(CombinedDir, "1_1_CombinedDat.RData"))
  unlink(file.path(CombinedDir, "1_1_CombinedDat_month.RData"))
  unlink(file.path(CombinedDir, "1_1_CombinedDat_wide.RData"))
  unlink(file.path(MalariaDir, "*_out.txt"))
})
