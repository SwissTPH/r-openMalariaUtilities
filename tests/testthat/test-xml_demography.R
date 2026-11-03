test_that("ageGroupsGen works", {
  actual <- ageGroupsGen(0, data.frame("poppercent" = 1, "upperbound" = 2))
  expected <- list(
    lowerbound = 0, group = list("poppercent" = 1, "upperbound" = 2)
  )

  expect_equal(actual, expected)
})

test_that("defineDemography works", {
  testlist <- list(demography = list())

  ## With growthRate
  expected <- list(
    demography = list(
      name = "foo",
      popSize = 3000L,
      maximumAgeYrs = 90,
      growthRate = 0.5,
      ageGroup = list(
        lowerbound = 1,
        group = list("poppercent" = 1, "upperbound" = 2)
      )
    )
  )

  actual <- defineDemography(
    baseList = testlist, name = "foo", popSize = 3000L, maximumAgeYrs = 90,
    growthRate = 0.5, lowerbound = 1, poppercent = 1, upperbound = 2
  )

  expect_equal(actual, expected)

  ## With placeholders
  expected <- list(
    demography = list(
      name = "foo",
      popSize = "@pop@",
      maximumAgeYrs = "@maxYears@",
      growthRate = "@growthRate@",
      ageGroup = list(
        lowerbound = "@lowerbound@",
        group = list("poppercent" = 1, "upperbound" = 2)
      )
    )
  )

  actual <- defineDemography(
    baseList = testlist, name = "foo", popSize = "@pop@",
    maximumAgeYrs = "@maxYears@", growthRate = "@growthRate@",
    lowerbound = "@lowerbound@", poppercent = 1, upperbound = 2
  )

  expect_equal(actual, expected)
})

test_that("write_demography_compat works", {
  testlist <- list(demography = list())

  ## With placeholders
  expected <- list(
    demography = list(
      name = "foo",
      popSize = "@pop@",
      maximumAgeYrs = 90,
      ageGroup = list(
        lowerbound = 0,
        group = list("poppercent" = 1, "upperbound" = 2)
      )
    )
  )

  actual <- write_demography_compat(
    baseList = testlist, country = "foo", pop = "@pop@", percents = 1,
    uppers = 2
  )

  expect_equal(actual, expected)
})

test_that("defineDemography preserves v44 input and rejects v49 growthRate", {
  args <- list(
    baseList = list(demography = list()), name = "test", popSize = 3000L,
    maximumAgeYrs = 90, lowerbound = 0, poppercent = 100, upperbound = 90
  )
  for (growthRate in list(NULL, 0, 0.5, "@growthRate@")) {
    args["growthRate"] <- list(growthRate)
    legacy <- do.call(defineDemography, args)
    args$baseList$OMVersion <- 44L
    v44 <- do.call(defineDemography, args)
    expect_identical(v44$demography, legacy$demography)
    expect_identical(v44$OMVersion, 44L)

    args$baseList$OMVersion <- 49L
    if (is.null(growthRate)) {
      v49 <- do.call(defineDemography, args)
      expect_identical(v49$demography, legacy$demography)
      expect_identical(v49$OMVersion, 49L)
      expect_false("growthRate" %in% names(v49$demography))
    } else {
      expect_error(
        do.call(defineDemography, args),
        "growthRate is not supported by OpenMalaria v49", fixed = TRUE
      )
    }
    args$baseList$OMVersion <- NULL
  }

  ## Validate the generated demography against each bundled schema. Other
  ## scenario sections are omitted from this section-level check only.
  args["growthRate"] <- list(NULL)
  for (version in c(44L, 49L)) {
    args$baseList$OMVersion <- version
    data <- do.call(defineDemography, args)
    root <- .makeXmlRoot(version, name = "test", analysisNo = NULL)
    .recXML(root, data["demography"], checkmate::makeAssertCollection())
    schema <- xml2::read_xml(system.file(
      "extdata", paste0("scenario_", version, ".xsd"),
      package = "openMalariaUtilities"
    ))
    xml2::xml_remove(xml2::xml_find_all(schema, paste0(
      "/xs:schema/xs:element[@name='scenario']/xs:complexType/xs:all/",
      "xs:element[@name!='demography']"
    )))
    valid <- xml2::xml_validate(xml2::read_xml(as.character(root)), schema)
    expect_true(valid, info = paste(attr(valid, "errors"), collapse = "\n"))
  }
})
