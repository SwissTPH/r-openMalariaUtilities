test_that("splitSeq works (no rest)", {
  actual <- splitSeq(1:20, 5)
  expected <- list(
    "1" = 1:5,
    "2" = 6:10,
    "3" = 11:15,
    "4" = 16:20
  )
  expect_equal(actual, expected)

  ## With rest
  actual <- splitSeq(1:21, 5)
  expected <- list(
    "1" = 1:5,
    "2" = 6:10,
    "3" = 11:15,
    "4" = 16:20,
    "5" = 21
  )
  expect_equal(actual, expected)
})

test_that("processFile works", {
  tmpPath <- file.path(.useDir(file.path(tempdir(), "test-utils")), "temp.txt")
  ## Create test file
  fcontent <- capture.output(cat("
foo

bar
bar
  baz
"))
  cat(fcontent, sep = "\n", file = tmpPath)

  ## Keep original
  actual <- processFile(f = tmpPath, trim = FALSE, rmdups = FALSE)
  expected <- fcontent
  expect_equal(actual, expected)

  ## Trim whitespace
  actual <- processFile(f = tmpPath, trim = TRUE, rmdups = FALSE)
  expected <- trimws(fcontent)
  expect_equal(actual, expected)

  ## Remove duplicates
  actual <- processFile(f = tmpPath, trim = FALSE, rmdups = TRUE)
  expected <- unique(fcontent)
  expected <- expected[expected != ""]
  expect_equal(actual, expected)

  ## Remove duplicates and trim whitespace
  actual <- processFile(f = tmpPath)
  expected <- unique(trimws(fcontent))
  expected <- expected[expected != ""]
  expect_equal(actual, expected)
})
