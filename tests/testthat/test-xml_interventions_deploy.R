test_that(".placeholderseqGen works", {
  expected <- list(
    foo = c(
      "@foo2000@", "@foo2001@", "@foo2002@", "@foo2003@", "@foo2004@",
      "@foo2005@", "@foo2006@", "@foo2007@", "@foo2008@", "@foo2009@",
      "@foo2010@"
    )
  )

  actual <- .placeholderseqGen(
    x = list(foo = list("foo", c(2000:2010))),
    placeholders = "foo"
  )

  expect_equal(actual, expected)
})

test_that("deployIT works", {
  testlist <- list(interventions = list())

  expected <- list(
    interventions = list(
      name = "All interventions",
      human = list(
        deployment = list(
          name = "ITN",
          component = list(
            id = "ITN"
          ),
          timed = list(
            deploy = list(
              coverage = "@foo2001@",
              time = "2001-03-15"
            ),
            deploy = list(
              coverage = "@foo2002@",
              time = "2002-03-15"
            )
          )
        )
      )
    )
  )

  actual <- deployIT(
    baseList = testlist, interval = list(
      years = c(2001:2002), months = 3, days = 15
    ),
    coverage = list("foo", c(2001:2002))
  )

  ## Normal
  expect_equal(actual, expected)

  ## minAge and maxAge
  expected <- list(
    interventions = list(
      name = "All interventions",
      human = list(
        deployment = list(
          name = "ITN",
          component = list(
            id = "ITN"
          ),
          timed = list(
            deploy = list(
              coverage = "@foo2001@",
              time = "2001-03-15",
              minAge = 1,
              maxAge = 5
            ),
            deploy = list(
              coverage = "@foo2002@",
              time = "2002-03-15",
              minAge = 1,
              maxAge = 5
            )
          )
        )
      )
    )
  )

  actual <- deployIT(
    baseList = testlist, interval = list(
      years = c(2001:2002), months = 3, days = 15
    ),
    coverage = list("foo", c(2001:2002)), minAge = 1, maxAge = 5
  )

  expect_equal(actual, expected)
})

test_that("deployIT requires paired, valid age limits", {
  ## Direct .recXML calls need the cache normally set up by .xmlMakeDocRec.
  putCache("placeholders", NULL)
  for (version in c(44L, 49L)) {
    build <- function(minAge = NULL, maxAge = NULL) {
      deployIT(list(OMVersion = version, interventions = list()),
               dates = c("2001-03-15", "2002-03-15"), coverage = 0.8,
               minAge = minAge, maxAge = maxAge)
    }
    for (limit in list(5, "@age@")) {
      expect_error(build(maxAge = limit),
                   "minAge and maxAge must be given together (or neither)", fixed = TRUE)
      expect_error(build(minAge = limit),
                   "minAge and maxAge must be given together (or neither)", fixed = TRUE)
    }
    for (invalid in list(-1, NA_real_, NaN, Inf, -Inf, numeric(), c(0, 1),
                         TRUE, "5", "@age@junk", "@@", NA_character_, character())) {
      expect_error(build(invalid, 5), "minAge", fixed = TRUE)
      expect_error(build(0, invalid), "maxAge", fixed = TRUE)
    }
    for (bounds in list(c(5, 5), c(6, 5), c(0, 0))) {
      expect_error(build(bounds[1], bounds[2]), "minAge must be less than maxAge",
                   fixed = TRUE)
    }
    expect_error(build(-1, "@max@"), "minAge", fixed = TRUE)
    expect_error(build("@min@", -1), "maxAge", fixed = TRUE)
    valid <- list(
      list(0, 5), list(0.25, 5), list("@min@", "@max@"),
      list(0, "@max@"), list("@min@", 5),
      list(c("@min1@", "@min2@"), "@max@")
    )
    for (bounds in valid) {
      output <- build(bounds[[1]], bounds[[2]])
      deployments <- output$interventions$human$deployment$timed
      for (i in seq_along(deployments)) {
        expectedMin <- bounds[[1]][min(i, length(bounds[[1]]))]
        expectedMax <- bounds[[2]][min(i, length(bounds[[2]]))]
        expect_identical(deployments[[i]]$minAge, expectedMin)
        expect_identical(deployments[[i]]$maxAge, expectedMax)
      }
      root <- .makeXmlRoot(version, name = "test", analysisNo = NULL)
      .recXML(root, output["interventions"], checkmate::makeAssertCollection())
      nodes <- xml2::xml_find_all(root, ".//timed/deploy")
      expect_identical(xml2::xml_attr(nodes, "minAge"),
                       as.character(rep(bounds[[1]], length.out = 2)))
      expect_identical(xml2::xml_attr(nodes, "maxAge"),
                       as.character(rep(bounds[[2]], length.out = 2)))
    }
    unrestricted <- build()
    expect_identical(build(NULL, NULL), unrestricted)
    for (deployment in unrestricted$interventions$human$deployment$timed) {
      expect_false("minAge" %in% names(deployment))
      expect_false("maxAge" %in% names(deployment))
    }
  }
  expect_error(
    deploy_IT(list(interventions = list()), dates = "2001-03-15", maxAge = 5),
    "minAge and maxAge must be given together (or neither)", fixed = TRUE
  )
  expect_error(
    deploy_it_compat(list(interventions = list()), coverage = 0.8,
                     y1 = 2021, y2 = 2021, m1 = 6, m2 = 7, d1 = 5, d2 = 5,
                     minAge = 1),
    "minAge and maxAge must be given together (or neither)", fixed = TRUE
  )
})

## DEPRECATED
test_that("deploy_it_compat works", {
  testlist <- list(interventions = list())

  ## Normal, dates generated
  expected <- list(
    interventions = list(
      name = "All interventions",
      human = list(
        deployment = list(
          name = "ITN",
          component = list(
            id = "ITN"
          ),
          timed = list(
            deploy = list(
              coverage = "futITNcov",
              time = as.Date("2021-06-05")
            ),
            deploy = list(
              coverage = "futITNcov",
              time = as.Date("2021-07-05")
            )
          )
        )
      )
    )
  )

  actual <- deploy_it_compat(
    testlist,
    component = "ITN",
    coverage = "futITNcov",
    byyear = FALSE,
    y1 = 2021, y2 = 2021, every = 1, interval = "month",
    m1 = 6, m2 = 7, d1 = 5, d2 = 5, SIMSTART = "1918-01-01"
  )

  expect_equal(actual, expected)

  ## deployvar used
  expected <- list(
    interventions = list(
      name = "All interventions",
      human = list(
        deployment = list(
          name = "ITN",
          component = list(
            id = "ITN"
          ),
          timed = list(
            deploy = list(
              coverage = "futITNcov",
              time = "@deploy_month2021@"
            )
          )
        )
      )
    )
  )

  actual <- deploy_it_compat(
    testlist,
    component = "ITN",
    coverage = "futITNcov",
    byyear = FALSE,
    deployvar = "deploy_month",
    y1 = 2021, y2 = 2021, every = 1, interval = "month",
    m1 = 6, m2 = 7, d1 = 5, d2 = 5, SIMSTART = "1918-01-01"
  )

  expect_equal(actual, expected)

  ## coverage placeholder
  expected <- list(
    interventions = list(
      name = "All interventions",
      human = list(
        deployment = list(
          name = "ITN",
          component = list(
            id = "ITN"
          ),
          timed = list(
            deploy = list(
              coverage = "@futITNcov2021@",
              time = "@deploy_month2021@"
            )
          )
        )
      )
    )
  )

  actual <- deploy_it_compat(
    testlist,
    component = "ITN",
    coverage = "@futITNcov@",
    byyear = TRUE,
    deployvar = "deploy_month",
    y1 = 2021, y2 = 2021, every = 1, interval = "month",
    m1 = 6, m2 = 7, d1 = 5, d2 = 5, SIMSTART = "1918-01-01"
  )

  expect_equal(actual, expected)

  ## cumulative
  expected <- list(
    interventions = list(
      name = "All interventions",
      human = list(
        deployment = list(
          name = "ITN",
          component = list(
            id = "ITN"
          ),
          timed = list(
            cumulativeCoverage = list(
              component = "ITN"
            ),
            deploy = list(
              coverage = "@futITNcov2021@",
              time = "@deploy_month2021@"
            )
          )
        )
      )
    )
  )

  actual <- deploy_it_compat(
    testlist,
    component = "ITN",
    coverage = "@futITNcov@",
    byyear = TRUE,
    deployvar = "deploy_month",
    y1 = 2021, y2 = 2021, every = 1, interval = "month",
    m1 = 6, m2 = 7, d1 = 5, d2 = 5, SIMSTART = "1918-01-01",
    cumulative = TRUE
  )

  expect_equal(actual, expected)

  ## cumulative + subpop
  expected <- list(
    interventions = list(
      name = "All interventions",
      human = list(
        deployment = list(
          name = "ITN",
          component = list(
            id = "ITN"
          ),
          timed = list(
            restrictToSubPop = list(
              id = "ITN-ITN"
            ),
            cumulativeCoverage = list(
              component = "ITN-ITN"
            ),
            deploy = list(
              coverage = "@futITNcov2021@",
              time = "@deploy_month2021@"
            )
          )
        )
      )
    )
  )

  actual <- deploy_it_compat(
    testlist,
    component = "ITN",
    coverage = "@futITNcov@",
    byyear = TRUE,
    deployvar = "deploy_month",
    y1 = 2021, y2 = 2021, every = 1, interval = "month",
    m1 = 6, m2 = 7, d1 = 5, d2 = 5, SIMSTART = "1918-01-01",
    cumulative = TRUE, subpop = TRUE
  )

  expect_equal(actual, expected)
})

test_that("deploy_cont_compat works", {
  testlist <- list(interventions = list())

  ## Normal, dates generated
  expected <- list(
    interventions = list(
      name = "All interventions",
      human = list(
        deployment = list(
          name = "IPTi",
          component = list(
            id = "IPTi"
          ),
          continuous = list(
            deploy = list(
              coverage = 0.8,
              targetAgeYrs = 0.8,
              begin = "2019-01-01",
              end = "2030-01-01"
            ),
            deploy = list(
              coverage = 0.7,
              targetAgeYrs = 0.16,
              begin = "2019-01-01",
              end = "2030-01-01"
            )
          )
        )
      )
    )
  )

  actual <- deploy_cont_compat(
    testlist,
    targetAgeYrs = c(0.8, 0.16),
    coverage = c(0.8, 0.7)
  )

  expect_equal(actual, expected)

  ## restrictToSubPop used
  expected <- list(
    interventions = list(
      name = "All interventions",
      human = list(
        deployment = list(
          name = "IPTi",
          component = list(
            id = "IPTi"
          ),
          continuous = list(
            restrictToSubPop = list(
              id = "foo1"
            ),
            deploy = list(
              coverage = 0.8,
              targetAgeYrs = 0.8,
              begin = "2019-01-01",
              end = "2030-01-01"
            ),
            deploy = list(
              coverage = 0.7,
              targetAgeYrs = 0.16,
              begin = "2019-01-01",
              end = "2030-01-01"
            )
          )
        )
      )
    )
  )

  actual <- deploy_cont_compat(
    testlist,
    targetAgeYrs = c(0.8, 0.16),
    coverage = c(0.8, 0.7),
    restrictToSubPop = "foo1"
  )

  expect_equal(actual, expected)

  ## vaccMinPrevDoses and vaccMaxCumDoses used
  expected <- list(
    interventions = list(
      name = "All interventions",
      human = list(
        deployment = list(
          name = "IPTi",
          component = list(
            id = "IPTi"
          ),
          continuous = list(
            deploy = list(
              coverage = 0.8,
              vaccMinPrevDoses = 0,
              vaccMaxCumDoses = 1,
              targetAgeYrs = 0.8,
              begin = "2019-01-01",
              end = "2030-01-01"
            ),
            deploy = list(
              coverage = 0.7,
              vaccMinPrevDoses = 1,
              vaccMaxCumDoses = 2,
              targetAgeYrs = 0.16,
              begin = "2019-01-01",
              end = "2030-01-01"
            )
          )
        )
      )
    )
  )

  actual <- deploy_cont_compat(
    testlist,
    targetAgeYrs = c(0.8, 0.16),
    coverage = c(0.8, 0.7),
    vaccMinPrevDoses = c(0, 1),
    vaccMaxCumDoses = c(1, 2)
  )

  expect_equal(actual, expected)
})
