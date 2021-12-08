test_that("storeScenarios works", {
  scenarios <- data.frame(pop = c(1:10))
  full <- "foo"
  assign("cacheDir", file.path(tempdir(), "cache"),
    envir = openMalariaUtilities:::.pkgcache
  )
  storeScenarios(scenarios = scenarios, full = full)

  actual <- file.exists(file.path(get(
    x = "cacheDir",
    envir = openMalariaUtilities:::.pkgcache
  ), "scens.RData"))
  expected <- TRUE

  expect_equal(actual, expected)
})

test_that(".scenariosRowSelect works (full range)", {
  scenarios <- data.frame(pop = c(1:10))
  actual <- seq_len(nrow(scenarios))
  expected <- .scenariosRowSelect(scenarios = scenarios)

  expect_equal(actual, expected)
})

test_that(".scenariosRowSelect works (custom range)", {
  scenarios <- data.frame(pop = c(1:10))
  actual <- 3:5
  expected <- .scenariosRowSelect(scenarios = scenarios, rowStart = 3, rowEnd = 5)

  expect_equal(actual, expected)
})
testthat::test_path("examples", "image.png")

test_that(".scenariosGenFiles works", {
  assign("cacheDir", file.path(tempdir(), "cache"),
    envir = openMalariaUtilities:::.pkgcache
  )
  assign("scenariosDir", file.path(tempdir(), "scenarios"),
    envir = openMalariaUtilities:::.pkgcache
  )
  assign("placeholders", "pop", envir = openMalariaUtilities:::.pkgcache)
  scenarios <- data.frame(pop = c(1:10))
  full <- "foo"

  unlink(get(x = "scenariosDir", envir = openMalariaUtilities:::.pkgcache),
    recursive = TRUE
  )
  dir.create(get(x = "scenariosDir", envir = openMalariaUtilities:::.pkgcache))

  .scenariosGenFiles(
    scenarios = scenarios,
    baseFile = testthat::test_path("ref", "exp_test_base.xml"),
    range = seq_len(nrow(scenarios)), placeholders = c("pop"),
    prefix = "exp_test"
  )

  scen_files <- paste0("exp_test_", c(1:10), ".xml")
  actual <- file.exists(file.path(get("scenariosDir",
    envir = openMalariaUtilities:::.pkgcache
  ), scen_files))
  expected <- rep(TRUE, 10)

  expect_equal(actual, expected)
})

test_that(".scenariosGenFiles emits error", {
  assign("cacheDir", file.path(tempdir(), "cache"),
    envir = openMalariaUtilities:::.pkgcache
  )
  assign("scenariosDir", file.path(tempdir(), "scenarios"),
    envir = openMalariaUtilities:::.pkgcache
  )
  assign("placeholders", "pop", envir = openMalariaUtilities:::.pkgcache)
  scenarios <- data.frame(pop = c(1:10))
  full <- "foo"

  unlink(get(x = "scenariosDir", envir = openMalariaUtilities:::.pkgcache),
    recursive = TRUE
  )
  dir.create(get(x = "scenariosDir", envir = openMalariaUtilities:::.pkgcache))

  expect_error(
    .scenariosGenFiles(
      scenarios = scenarios,
      baseFile = testthat::test_path("ref", "exp_test_base.xml"),
      range = seq_len(nrow(scenarios)), placeholders = c("foo"),
      prefix = "exp_test"
    ),
    "variables are definded in the base xml file but not in the scenarios"
  )
})

test_that(".scenariosGenFiles emits warning", {
  assign("cacheDir", file.path(tempdir(), "cache"),
    envir = openMalariaUtilities:::.pkgcache
  )
  assign("scenariosDir", file.path(tempdir(), "scenarios"),
    envir = openMalariaUtilities:::.pkgcache
  )
  assign("placeholders", "pop", envir = openMalariaUtilities:::.pkgcache)
  scenarios <- data.frame(pop = c(1:10), foo = c(1:10))
  full <- "foo"

  unlink(get(x = "scenariosDir", envir = openMalariaUtilities:::.pkgcache),
    recursive = TRUE
  )
  dir.create(get(x = "scenariosDir", envir = openMalariaUtilities:::.pkgcache))

  expect_warning(
    .scenariosGenFiles(
      scenarios = scenarios,
      baseFile = testthat::test_path("ref", "exp_test_base.xml"),
      range = c(1:2), placeholders = c("pop", "foo"),
      prefix = "exp_test"
    ),
    "variables are not used in the base xml file but definded in the scenarios"
  )
})

test_that("generateScenarios works", {
  assign("cacheDir", file.path(tempdir(), "cache"),
    envir = openMalariaUtilities:::.pkgcache
  )
  assign("scenariosDir", file.path(tempdir(), "scenarios"),
    envir = openMalariaUtilities:::.pkgcache
  )
  assign("placeholders", "pop", envir = openMalariaUtilities:::.pkgcache)
  scenarios <- data.frame(pop = c(1:10))
  full <- "foo"

  unlink(get(x = "scenariosDir", envir = openMalariaUtilities:::.pkgcache),
    recursive = TRUE
  )
  dir.create(get(x = "scenariosDir", envir = openMalariaUtilities:::.pkgcache))

  generateScenarios(
    scenarios = scenarios, full = full,
    baseFile = testthat::test_path("ref", "exp_test_base.xml"),
    prefix = "exp_test"
  )

  scen_files <- paste0("exp_test_", c(1:10), ".xml")
  actual <- file.exists(file.path(get(
    x = "scenariosDir",
    envir = openMalariaUtilities:::.pkgcache
  ), scen_files))
  expected <- rep(TRUE, 10)

  expect_equal(actual, expected)
})
