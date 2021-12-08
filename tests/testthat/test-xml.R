test_that(".xmlAddChunks works", {
  actual <- .xmlAddChunks(
    outlist = list(),
    element = "foo",
    attributeList = list("bar" = 1, "baz" = 1)
  )
  expected <- list(foo = list("bar" = 1, "baz" = 1))

  expect_equal(actual, expected)
})

test_that(".xmlAddChunks throws wrong length error", {
  expect_error(
    .xmlAddChunks(
      outlist = list(),
      element = "foo",
      attributeList = list("bar" = c(1, 2), "baz" = 1)
    ),
    "Value vectors in attributeList need to be of same length"
  )
})


test_that(".placeholderCollect works", {
  assign("placeholders", c("foo"), envir = openMalariaUtilities:::.pkgcache)
  .placeholderCollect("@bar@")
  actual <- get("placeholders", envir = openMalariaUtilities:::.pkgcache)
  expectedHash <- hash::hash()
  expectedHash$placeholders <- c("bar", "foo", expectedHash$placeholders)
  expected <- expectedHash$placeholders

  expect_equal(actual, expected)
})

test_that(".placeholderCollect handles duplicates", {
  assign("placeholders", c("bar", "foo", "bar", "foo", "foo"), envir = openMalariaUtilities:::.pkgcache)
  .placeholderCollect("@bar@")
  actual <- get("placeholders", envir = openMalariaUtilities:::.pkgcache)
  expectedHash <- hash::hash()
  expectedHash$placeholders <- c("bar", "foo", expectedHash$placeholders)
  expected <- expectedHash$placeholders

  expect_equal(actual, expected)
})

test_that("recXML works", {
  expected <- xml2::read_xml(
    "
<demography maximumAgeYrs=\"90\" name=\"Ifakara\" popSize=\"1000\">
<ageGroup lowerbound=\"0\"></ageGroup>
</demography>
"
  )
  actual <- recXML(
    xml2::read_xml("<root> test </root>"),
    list(
      demography = list(
        maximumAgeYrs = 90,
        name = "Ifakara",
        popSize = 1000L,
        ageGroup = list(
          lowerbound = 0
        )
      )
    ),
    checkmate::makeAssertCollection(),
    list()
  )

  expect_equal(actual, expected)
})

test_that(".xmlMakeDocRec works", {
  expected <- xml2::read_xml(
    "
<root>
<demography maximumAgeYrs=\"90\" name=\"Ifakara\" popSize=\"1000\">
<ageGroup lowerbound=\"0\"></ageGroup>
</demography>
</root>
"
  )
  actual <- .xmlMakeDocRec(
    xml2::read_xml("<root/>"),
    list(
      demography = list(
        maximumAgeYrs = 90,
        name = "Ifakara",
        popSize = 1000L,
        ageGroup = list(
          lowerbound = 0
        )
      )
    )
  )

  expect_equal(actual, expected)
})
