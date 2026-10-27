test_that("defineEntomology accepts numeric and placeholder seasonality vectors", {
  mosquitoParameters <- list("Anopheles gambiae" = list(
    mosqRestDuration = list(value = 3),
    extrinsicIncubationPeriod = list(value = 11),
    mosqLaidEggsSameDayProportion = list(value = 0.313),
    mosqSeekingDuration = list(value = 0.33),
    mosqSurvivalFeedingCycleProbability = list(value = 0.623),
    mosqProbBiting = list(mean = 0.95, variance = 0),
    mosqProbFindRestSite = list(mean = 0.95, variance = 0),
    mosqProbResting = list(mean = 0.99, variance = 0),
    mosqProbOvipositing = list(mean = 0.88),
    mosqHumanBloodIndex = list(mean = 0.939)
  ))
  for (version in list(NULL, 44L, 49L)) {
    for (n in c(12L, 365L)) {
      placeholders <- paste0("@", if (n == 12L) "m" else "d", seq_len(n), "@")
      for (values in list(seq_len(n), placeholders)) {
        seasonalityParameters <- list("Anopheles gambiae" = list(
          annualEIR = "@EIR@", seasonality = values,
          propInfected = 0.078, propInfectious = 0.021
        ))
        output <- defineEntomology(
          list(OMVersion = version, entomology = list()),
          seasonalityParameters, mosquitoParameters
        )
        seasonality <- output$entomology$vector$anopheles$seasonality
        entry <- if (n == 12L) "monthlyValues" else "dailyValues"
        series <- seasonality[[entry]]
        expect_identical(seasonality$annualEIR, "@EIR@")
        expect_identical(unlist(series[names(series) == "value"], use.names = FALSE), values)

        ## Requiring every value to be a placeholder prevents accepting
        ## malformed entries just because the first value is valid.
        invalid <- placeholders
        invalid[n] <- "not a placeholder"
        seasonalityParameters[[1]]$seasonality <- invalid
        expect_error(
          defineEntomology(list(OMVersion = version, entomology = list()),
                           seasonalityParameters, mosquitoParameters),
          "vector of placeholders", fixed = TRUE
        )
      }
    }
    for (n in c(11L, 366L)) {
      seasonalityParameters[[1]]$seasonality <- paste0("@x", seq_len(n), "@")
      expect_error(
        defineEntomology(list(OMVersion = version, entomology = list()),
                         seasonalityParameters, mosquitoParameters),
        "vector of placeholders", fixed = TRUE
      )
    }
  }
})

test_that("make_ento_compat works", {
  testlist <- list(entomology = list())

  seasonality <- paste0("@m", 1:12, "@")
  monthlyValues <- list(
    smoothing = "fourier"
  )
  for (i in seasonality) {
    monthlyValues <- append(monthlyValues, list(value = list(i)))
  }

  expected <- list(
    entomology = list(
      mode = "dynamic",
      name = "Namawala",
      scaledAnnualEIR = "@EIR@",
      vector = list(
        anopheles = list(
          mosquito = "gambiaesl_indoor",
          propInfected = 0.078,
          propInfectious = 0.021,
          seasonality = list(
            input = "EIR",
            annualEIR = 0.81,
            monthlyValues = monthlyValues
          ),
          mosq = list(
            minInfectedThreshold = 0.001,
            mosqRestDuration = list(
              value = 3
            ),
            extrinsicIncubationPeriod = list(
              value = 11
            ),
            mosqLaidEggsSameDayProportion = list(
              value = 0.313
            ),
            mosqSeekingDuration = list(
              value = 0.33
            ),
            mosqSurvivalFeedingCycleProbability = list(
              value = 0.623
            ),
            availability = list(
              distr = "const"
            ),
            mosqProbBiting = list(
              mean = 0.95,
              variance = 0
            ),
            mosqProbFindRestSite = list(
              mean = 0.95,
              variance = 0
            ),
            mosqProbResting = list(
              mean = 0.99,
              variance = 0
            ),
            mosqProbOvipositing = list(
              value = 0.88
            ),
            mosqHumanBloodIndex = list(
              value = 0.939
            )
          ),
          nonHumanHosts = list(
            name = "unprotectedAnimals",
            mosqRelativeEntoAvailability = list(
              value = 1
            ),
            mosqProbBiting = list(
              value = 0.95
            ),
            mosqProbFindRestSite = list(
              value = 0.95
            ),
            mosqProbResting = list(
              value = 0.99
            )
          )
        ),
        nonHumanHosts = list(
          name = "unprotectedAnimals",
          number = 1.0
        )
      )
    )
  )
  actual <- make_ento_compat(baseList = testlist, mosqs = c("gambiaesl_indoor"))

  ## Normal
  expect_equal(actual, expected)
})


test_that("defineEntomology works", {
  testlist <- list(entomology = list())

  monthlyValues <- list(
    smoothing = "fourier"
  )
  for (i in c(5, 4, 3, 2, 1, 2, 3, 5, 8, 13, 11, 7)) {
    monthlyValues <- append(monthlyValues, list(value = list(i)))
  }

  expected <- list(
    entomology = list(
      mode = "dynamic",
      name = "Namawala",
      scaledAnnualEIR = 23,
      vector = list(
        anopheles = list(
          mosquito = "gambiae_ss",
          propInfected = 0.078,
          propInfectious = 0.021,
          seasonality = list(
            annualEIR = 0.8,
            input = "EIR",
            monthlyValues = monthlyValues
          ),
          mosq = list(
            minInfectedThreshold = 0.001,
            mosqRestDuration = list(
              value = 3
            ),
            extrinsicIncubationPeriod = list(
              value = 11
            ),
            mosqLaidEggsSameDayProportion = list(
              value = 0.313
            ),
            mosqSeekingDuration = list(
              value = 0.33
            ),
            mosqSurvivalFeedingCycleProbability = list(
              value = 0.623
            ),
            availability = list(
              distr = "const"
            ),
            mosqProbBiting = list(
              mean = 0.95,
              variance = 0
            ),
            mosqProbFindRestSite = list(
              mean = 0.95,
              variance = 0
            ),
            mosqProbResting = list(
              mean = 0.99,
              variance = 0
            ),
            mosqProbOvipositing = list(
              value = 0.88
            ),
            mosqHumanBloodIndex = list(
              value = 0.939
            )
          ),
          nonHumanHosts = list(
            name = "unprotectedAnimals",
            mosqRelativeEntoAvailability = list(
              value = 1
            ),
            mosqProbBiting = list(
              value = 0.95
            ),
            mosqProbFindRestSite = list(
              value = 0.95
            ),
            mosqProbResting = list(
              value = 0.99
            )
          )
        ),
        nonHumanHosts = list(
          name = "unprotectedAnimals",
          number = 1.0
        )
      )
    )
  )
  args <- list(
    baseList = testlist,
    seasonalityParameters = list(
      "gambiae_ss" = list(
        seasonality = c(5, 4, 3, 2, 1, 2, 3, 5, 8, 13, 11, 7),
        annualEIR = 0.8,
        propInfected = 0.078,
        propInfectious = 0.021
      )
    ),
    mosquitoParameters = list(
      "gambiae_ss" = list(
        mosqRestDuration = list(value = 3),
        extrinsicIncubationPeriod = list(value = 11),
        mosqLaidEggsSameDayProportion = list(value = 0.313),
        mosqSeekingDuration = list(value = 0.33),
        mosqSurvivalFeedingCycleProbability = list(value = 0.623),
        mosqProbBiting = list(
          mean = 0.95,
          variance = 0
        ),
        mosqProbFindRestSite = list(
          mean = 0.95,
          variance = 0
        ),
        mosqProbResting = list(
          mean = 0.99,
          variance = 0
        ),
        mosqProbOvipositing = list(mean = 0.88),
        mosqHumanBloodIndex = list(mean = 0.939)
      )
    ),
    scaledAnnualEIR = 23
  )

  ## Normal
  actual <- do.call(defineEntomology, args)
  expect_equal(actual, expected)

  ## Explicit v44 input retains the exact legacy entomology.
  args$baseList$OMVersion <- 44L
  v44 <- do.call(defineEntomology, args)
  expect_identical(v44$entomology, actual$entomology)
  expect_identical(v44$OMVersion, 44L)

  ## v49 only removes the two obsolete initial-guess attributes.
  args$baseList$OMVersion <- 49L
  v49 <- do.call(defineEntomology, args)
  expectedV49 <- expected$entomology
  expectedV49$vector$anopheles$propInfected <- NULL
  expectedV49$vector$anopheles$propInfectious <- NULL
  expect_identical(v49$entomology, expectedV49)
  expect_identical(v49$OMVersion, 49L)

  ## Callers can omit the obsolete inputs entirely for v49.
  args$seasonalityParameters$gambiae_ss$propInfected <- NULL
  args$seasonalityParameters$gambiae_ss$propInfectious <- NULL
  expect_identical(do.call(defineEntomology, args), v49)

  ## The generated XML must contain no NULL attributes or removed inputs.
  root <- .makeXmlRoot(49L, name = "test", analysisNo = NULL)
  .xmlMakeDocRec(root, v49)
  anopheles <- xml2::xml_find_all(root, ".//anopheles")
  expect_length(anopheles, 1L)
  expect_identical(xml2::xml_attr(anopheles, "mosquito"), "gambiae_ss")
  expect_true(is.na(xml2::xml_attr(anopheles, "propInfected")))
  expect_true(is.na(xml2::xml_attr(anopheles, "propInfectious")))

  ## Validate both generated sections against the bundled entomology types.
  ## Other scenario sections are omitted from this section-level check only.
  for (data in list(v44, v49)) {
    version <- data$OMVersion
    root <- .makeXmlRoot(version, name = "test", analysisNo = NULL)
    .recXML(root, data["entomology"], checkmate::makeAssertCollection())
    schema <- xml2::read_xml(system.file(
      "extdata", paste0("scenario_", version, ".xsd"),
      package = "openMalariaUtilities"
    ))
    xml2::xml_remove(xml2::xml_find_all(schema, paste0(
      "/xs:schema/xs:element[@name='scenario']/xs:complexType/xs:all/",
      "xs:element[@name!='entomology']"
    )))
    valid <- xml2::xml_validate(xml2::read_xml(as.character(root)), schema)
    expect_true(valid, info = paste(attr(valid, "errors"), collapse = "\n"))
  }
})
