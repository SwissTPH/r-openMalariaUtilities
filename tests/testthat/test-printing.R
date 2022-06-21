test_that("toggleVerbose works", {
  ## On
  toggleVerbose()
  expected <- TRUE
  actual <- get("verboseOutput", envir = openMalariaUtilities:::.pkgenv)
  expect_equal(actual, expected)

  ## Off
  toggleVerbose()
  expected <- FALSE
  actual <- get("verboseOutput", envir = openMalariaUtilities:::.pkgenv)
  expect_equal(actual, expected)
})

test_that("toggleDebug works", {
  ## On
  toggleDebug()
  expected <- TRUE
  actual <- get("debugOutput", envir = openMalariaUtilities:::.pkgenv)
  expect_equal(actual, expected)

  ## Off
  toggleDebug()
  expected <- FALSE
  actual <- get("debugOutput", envir = openMalariaUtilities:::.pkgenv)
  expect_equal(actual, expected)
})

test_that(".printVerbose works", {
  assign("verboseOutput", TRUE, envir = openMalariaUtilities:::.pkgenv)
  expect_output(.printVerbose("foo"))
})

test_that(".printDebug works", {
  assign("debugOutput", TRUE, envir = openMalariaUtilities:::.pkgenv)
  expect_output(.printDebug("foo"))
})

## Switch everthing off again
assign("debugOutput", FALSE, envir = openMalariaUtilities:::.pkgenv)
assign("verboseOutput", FALSE, envir = openMalariaUtilities:::.pkgenv)
