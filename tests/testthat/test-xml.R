test_that(".xmlAddChunks works", {
  actual <- .xmlAddChunks(
    outlist = list(),
    element = "foo",
    attributeList = list("bar" = 1, "baz" = 1)
  )
  expected <- list(foo = list("bar" = 1, "baz" = 1))

  expect_equal(actual, expected)

  ## .xmlAddChunks throws wrong length error
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
  putCache("placeholders", c("foo"))
  .placeholderCollect("@bar@")
  actual <- getCache("placeholders")
  expected <- c("bar", "foo")

  expect_equal(actual, expected)

  ## .placeholderCollect handles duplicates
  putCache("placeholders", c("bar", "foo", "bar", "foo", "foo"))
  .placeholderCollect("@bar@")
  actual <- getCache("placeholders")
  expected <- c("bar", "foo")

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


test_that(".xmlAddList works", {

  ## Not appending
  testlist <- list(
    interventions = list(
      human = list(
        component = list("foo"), deployment = list("bar"),
        component = list("foo2"), deployment = list("bar2")
      )
    )
  )

  expected <- list(
    interventions = list(
      human = list(
        component = list(
          id = "foo3",
          name = "foo3",
          GVI = list(
            decay = 0.25,
            "function" = "step"
          )
        ),
        deployment = list("bar"),
        deployment = list("bar2")
      )
    )
  )

  actual <- .xmlAddList(testlist, c("interventions", "human"), "component",
    list(
      id = "foo3",
      name = "foo3",
      GVI = list(
        decay = 0.25,
        "function" = "step"
      )
    ),
    append = FALSE
  )

  expect_equal(sort(unlist(actual)), sort(unlist(expected)))

  ## .xmlAddList works (appending)
  testlist <- list(
    interventions = list(
      human = list(
        component = list("foo"), deployment = list("bar"),
        component = list("foo2"), deployment = list("bar2")
      )
    )
  )

  expected <- list(
    interventions = list(
      human = list(
        component = list(
          id = "foo3",
          name = "foo3",
          GVI = list(
            decay = 0.25,
            "function" = "step"
          )
        ),
        component = list("foo"),
        deployment = list("bar"),
        component = list("foo2"),
        deployment = list("bar2")
      )
    )
  )

  actual <- .xmlAddList(
    testlist, c("interventions", "human"), "component",
    list(
      id = "foo3",
      name = "foo3",
      GVI = list(
        decay = 0.25,
        "function" = "step"
      )
    )
  )

  expect_equal(sort(unlist(actual)), sort(unlist(expected)))
})
