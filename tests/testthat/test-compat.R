test_that(".warn_about_bad_names works", {
  dat <- data.frame(setting = "alpha", pop = 500, sdVar = 0.5)
  expect_error(.warn_about_bad_names(dat))

  dat <- data.frame(setting = "alpha", X = 500)
  out <- .warn_about_bad_names(dat)
  expect_equal(colnames(out), "setting")

  dat <- data.frame(setting = "alpha", pop = 500)
  out <- .warn_about_bad_names(dat)
  expect_equal(colnames(out), "setting")

  dat <- data.frame(setting = "alpha__2", sd = 500)
  expect_error(.warn_about_bad_names(dat))

  dat <- data.frame(setting = "alpha", sub = c("a 1", "a2", "a 3"))
  expect_error(.warn_about_bad_names(dat))
})

test_that("import_countrydat works", {
  prev <- data.frame(
    setting = "alpha",
    m1 = 5,
    pop = 500
  )

  write.csv(prev, file = "test.csv")

  out <- import_countrydat("test.csv")
  expect_equal(colnames(out), c("setting", "m1"))
  unlink("test.csv")
})


test_that("convert_cm works as expected", {
  expect_equal(convert_cm(c(0.5, 0.8), katya = FALSE, reverse = T), c(0.778, 0.951))
  expect_equal(convert_cm(orig = c(0, 1), katya = FALSE), c(0, 0.985))
  expect_error(convert_cm(orig = c(0.5, 1, 2)), class = "simpleError")
  expect_error(convert_cm(0.5, country = "fake"))
  out <- convert_cm(0.5, country = "UGA", scale = NULL)
  expect_equal(out, 0.175)

  out <- convert_cm(0.5, country = "UGA", scale = 1)
  expect_equal(out, 0.242)

  out <- convert_cm(0.5, scale = 1)
  expect_equal(out, 0.242)

  out <- convert_cm(0.242, scale = 1, reverse = TRUE)
  expect_equal(out, 0.505)
})

test_that("convert_access works as expected", {

  #-- number not in range
  dat <- data.frame(setting = "alpha", access2005 = .5, acceSS2006 = .1, access2010 = 1.5)
  expect_error(convert_access(dat, pattern = "acc", katya = T, country = NULL))

  #-- expect the following
  dat <- data.frame(setting = "alpha", access2005 = .5, acceSS2006 = .1, access2010 = 1)
  out <- convert_access(dat, pattern = "acc", katya = T, country = "MOZ")

  expect_equal(out$scaled_down_flag, TRUE)
  expect_equal(out$access2010, 0.362)

  #-- expect the following
  dat <- data.frame(setting = "alpha", access2005 = .5, acceSS2006 = .1, access2010 = 1)
  expect_error(convert_access(dat, pattern = "acc", katya = T, country = "USA"))

  #-- expect the following
  dat <- data.frame(
    setting = "alpha", access2005 = .5, acceSS2006 = .1,
    access2010 = 1, scaled_down_flag = TRUE
  )

  expect_error(convert_access(dat, pattern = "acc", katya = T, country = "MOZ"))
})

test_that(".extract_param_names works as expected", {
  full <- .create_test_full()
  scens <- .create_test_scens()
  params <- .extract_param_names(full, scens)

  uni <- names(full)[!(names(full) %in% c("pop"))]

  expect_equal(params$ future_variables, c("futITNcov", "futIRScov"))
  expect_equal(params$ historical_variables, c("EIR", "seed"))
  expect_equal(params$ unique_variables, uni)
})

test_that(".assign_id_variables works as expected", {
  temp <- .extract_param_names(
    full = .create_test_full(),
    scens = .create_test_scens(),
    models = "models",
    seed = "seed",
    fut = "fut",
    placeholder = NULL,
    seed_as_hist_param = T,
    include = NULL
  )

  temp2 <- .assign_id_variables(
    CombinedDat_wide = .create_test_CombinedDat_Aggr(),
    unique_variables = temp$ unique_variables,
    historical_variables = temp$ historical_variables,
    future_variables = temp$ future_variables
  )

  temp2 <- temp2[, c("FutScen", "fut", "HistScen_nr", "HistScen", "UniqueScenario", "setting_future")]

  answer <- read.delim(text = "nr	FutScen	fut	HistScen_nr	HistScen	UniqueScenario	setting_future
3	0.65__0	1	1	25__1	0.65__0__25__alpha__1	alpha_futnr_1
3	0.65__0	1	1	25__1	0.65__0__25__alpha__1	alpha_futnr_1
3	0.65__0	1	1	25__1	0.65__0__25__alpha__1	alpha_futnr_1
3	0.65__0	1	1	25__1	0.65__0__25__alpha__1	alpha_futnr_1
4	0.65__0	1	2	5__1	0.65__0__5__alpha__1	alpha_futnr_1
4	0.65__0	1	2	5__1	0.65__0__5__alpha__1	alpha_futnr_1
4	0.65__0	1	2	5__1	0.65__0__5__alpha__1	alpha_futnr_1
4	0.65__0	1	2	5__1	0.65__0__5__alpha__1	alpha_futnr_1
1	0.65__0.8	2	1	25__1	0.65__0.8__25__alpha__1	alpha_futnr_2
1	0.65__0.8	2	1	25__1	0.65__0.8__25__alpha__1	alpha_futnr_2
1	0.65__0.8	2	1	25__1	0.65__0.8__25__alpha__1	alpha_futnr_2
1	0.65__0.8	2	1	25__1	0.65__0.8__25__alpha__1	alpha_futnr_2
2	0.65__0.8	2	2	5__1	0.65__0.8__5__alpha__1	alpha_futnr_2
2	0.65__0.8	2	2	5__1	0.65__0.8__5__alpha__1	alpha_futnr_2
2	0.65__0.8	2	2	5__1	0.65__0.8__5__alpha__1	alpha_futnr_2
2	0.65__0.8	2	2	5__1	0.65__0.8__5__alpha__1	alpha_futnr_2", sep = "\t", header = T)

  # removing 'nr' column
  testthat::expect_equal(temp2, answer[, -1] %>% dplyr::arrange(fut, HistScen_nr))
})

test_that("enclose yields a vector of text", {
  expect_equal(enclose("blah"), "c('blah')")
  expect_equal(enclose(c("1", "2")), "c('1','2')")
})

test_that("add_idvars works", {
  assign(x = "experimentDir", value = tempdir(), envir = openMalariaUtilities:::.pkgcache)
  scens <- .create_test_scens()
  full <- .create_test_full()
  out <- add_idvars(scens, full, overwrite = T, save = F, confirm = TRUE)

  expect_equal(out$ setting_future, paste0("alpha_futnr_", c(1, 1, 2, 2)))
  expect_equal(colnames(out), c("nr", "FutScen", "fut", "HistScen_nr", "HistScen", "setting", "UniqueScenario", "pop", "EIR", "seed", "futITNcov", "futIRScov", "setting_future"))
  expect_equal(out$fut, c(1, 1, 2, 2))
  expect_equal(out$HistScen_nr, c(1, 2, 1, 2))
})

test_that("assign_value works", {
  scens <- data.frame(futITNcov = c("high", "none", "curr"), setting = "alpha")
  scens <- assign_value(scens = scens, variable = "futITNcov", levels = c("high", "curr", "none"), values = c(.80, .25, 0))
  expect_equal(as.numeric(scens$ futITNcov), c(0.8, 0, .25))
})

test_that(".add_file_column_to_scens works", {
  scens <- .create_test_scens()
  out <- .add_file_column_to_scens(scens, nameExperiment = "test", startnum = 1)
  expect_equal(out$file, paste0("wutest_", 1:4, ".xml"))
})

test_that("write_scen_data works", {
  assign(x = "experimentDir", value = tempdir(), envir = openMalariaUtilities:::.pkgcache)
  assign(x = "cacheDir", value = tempdir(), envir = openMalariaUtilities:::.pkgcache)
  assign(x = "experimentName", value = "test", envir = openMalariaUtilities:::.pkgcache)

  scens <- .create_test_scens()
  full <- .create_test_full()
  out <- write_scen_data(scens, full, nameExperiment = "test", saveit = F, MainDir = getwd())
  expect_equal(colnames(out), c("nr", "FutScen", "fut", "HistScen_nr", "HistScen", "setting", "UniqueScenario", "pop", "file", "EIR", "seed", "futITNcov", "futIRScov", "setting_future"))
  expect_equal(dim(out), c(4, 14))

  expect_equal(out$fut, c(1, 1, 2, 2))
  expect_equal(out$HistScen_nr, rep(1:2, times = 2))
})

test_that("make_mosquito_name works", {
  out <- make_mosquito_name(mosq = c("one", "two"), inout = TRUE)
  expect_equal(out, c("one_indoor", "two_indoor", "one_outdoor", "two_outdoor"))
})

test_that(".seed_warning works", {
  scens <- data.frame(seed = 1:2, setting = "alpha")
  expect_warning(.seed_warning(scens, seed_as_hist_param = F))

  scens <- data.frame(seed = 1:2, setting = "alpha")
  expect_true(.seed_warning(scens, seed_as_hist_param = T))
})

test_that(".identify_files_to_join works", {
  dir <- file.path(tempdir(), "test_identify_files_to_join")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  CombinedDat_wide <- .create_test_CombinedDat_wide()
  save(CombinedDat_wide, file = file.path(dir, "1_1_CombinedDat_wide.RData"))
  save(CombinedDat_wide, file = file.path(dir, "1_2_CombinedDat_wide.Rdata"))

  out <- .identify_files_to_join(dir,
    setting_number = 1,
    widename = "_CombinedDat_wide.RData"
  )

  expect_equal(out$tempname, file.path(dir, "1_CombinedDat_wide.RData"))
})
