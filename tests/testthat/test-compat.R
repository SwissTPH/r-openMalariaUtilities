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
