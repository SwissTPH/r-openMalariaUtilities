test_that("v49 dictionaries contain the additional default output measures", {
  legacy <- omOutputDict()
  expect_identical(omOutputDict(version = 44L), legacy)
  v49 <- omOutputDict(version = 49L)
  expect_identical(anyDuplicated(v49$measure_index), 0L)
  expect_identical(anyDuplicated(v49$measure_name), 0L)
  expect_identical(names(v49), names(legacy))
  expect_identical(
    legacy$measure_name[legacy$measure_index == 43L], "nNewefections"
  )
  expect_identical(v49$measure_name[v49$measure_index == 43L], "nNewInfections")

  ## IDs and names from schema-49.0/model/mon/OutputMeasures.h.
  ids <- c(41L, 42L, 44L, 45L, 46L, 80L, 81L, 82L)
  labels <- c(
    "Clinical_FirstDayDeaths", "Clinical_HospitalFirstDayDeaths",
    "nMassITNs", "nEPI_ITNs", "nMassIRS", "nCMDTReport",
    "nSevereWithoutComorbidities", "expectedSevereWithoutComorbidities"
  )
  expect_identical(v49$measure_name[match(ids, v49$measure_index)], labels)
  added <- v49[match(ids, v49$measure_index)]
  expect_true(all(added$age_group & added$cohort & added$aggregated))
  expect_false(any(added$genotype | added$vector_species | added$drug_ID))

  baseIds <- c(1L, 3L, 6L, 8L, 14L, 43L)
  baseLabels <- c(
    "nInfect", "nPatent", "totalInfs", "totalPatentInf", "nUncomp",
    "nNewInfections"
  )
  origins <- c("Imported", "Introduced", "Indigenous")
  for (i in seq_along(origins)) {
    variants <- v49[match(1000L * i + baseIds, v49$measure_index)]
    expect_identical(variants$measure_name, paste0(baseLabels, "_", origins[i]))
    expect_identical(variants$genotype, c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE))
    expect_identical(variants$aggregated, c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE))
  }
  expect_identical(
    v49$measure_name[match(c(2036L, 3036L), v49$measure_index)],
    c("simulatedEIR_Introduced", "simulatedEIR_Indigenous")
  )
  expect_false(1036L %in% v49$measure_index)
  expect_length(setdiff(v49$measure_index, legacy$measure_index), 28L)
  expect_error(omOutputDict(version = 45))

  ## Requests for v49 must never mutate the default v44 dictionary.
  expect_identical(omOutputDict(), legacy)
})

test_that("readOutputFile resolves measure versions and preserves raw output", {
  clearCache()
  f <- tempfile(fileext = ".txt")
  writeLines(c("1\t1\t43\t7", "1\t2\t0\t12"), f)

  legacy <- readOutputFile(f, translate = "measures")
  expect_identical(legacy$measure, c("nNewefections", "nHost"))
  putCache("OMVersion", 49L)
  v49 <- readOutputFile(f, translate = "measures", scenID = 9L)
  expect_identical(v49$measure, c("nNewInfections", "nHost"))
  expect_identical(v49$value, c(7L, 12L))
  expect_identical(v49$scenario_id, c(9L, 9L))
  expect_identical(
    readOutputFile(f, translate = "measures", version = 44L), legacy
  )
  putCache("OMVersion", 44L)
  expect_identical(
    readOutputFile(f, translate = "measures", version = 49L)$measure,
    c("nNewInfections", "nHost")
  )
  raw <- readOutputFile(f, translate = FALSE)
  expect_identical(raw$measure, c(43L, 0L))
  expect_identical(raw$value, legacy$value)
  expect_error(readOutputFile(f, translate = "measures", version = 45))
  clearCache()
})

test_that("new v49 measures translate and unknown IDs remain identifiable", {
  f <- tempfile(fileext = ".txt")
  writeLines(c(
    "1\t1\t1043\t2", "1\t1\t80\t3", "1\t1\t82\t0.5",
    "1\t1\t9999\t4", "1\t1\t9999\t5"
  ), f)
  expect_warning(
    output <- readOutputFile(f, translate = "measures", version = 49L),
    "Unknown OpenMalaria measure IDs: 9999", fixed = TRUE
  )
  expect_identical(output$measure, c(
    "nNewInfections_Imported", "nCMDTReport",
    "expectedSevereWithoutComorbidities", "9999", "9999"
  ))
  expect_identical(output$value, c(2, 3, 0.5, 4, 5))
  expect_false(anyNA(output$measure))
  expect_identical(nrow(output), 5L)
  expect_warning(raw <- readOutputFile(f, translate = FALSE, version = 49L), NA)
  expect_identical(raw$measure, c(1043L, 80L, 82L, 9999L, 9999L))

  ## The same unknown-ID safeguard applies to legacy files.
  writeLines("1\t1\t9999\t4", f)
  expect_warning(
    legacy <- readOutputFile(f, translate = "measures", version = 44L),
    "Unknown OpenMalaria measure IDs: 9999", fixed = TRUE
  )
  expect_identical(legacy$measure, "9999")
})

test_that("collectResults stores cached v49 labels without database changes", {
  clearCache()
  rootDir <- tempfile("output-translation-")
  dir.create(rootDir)
  setupDirs("v49", rootDir = rootDir, replace = TRUE)
  putCache("OMVersion", 49L)
  putCache("placeholders", character(0))
  putCache("surveyTimes", data.table::data.table(
    number = 1L, date = as.Date("2000-01-01")
  ))
  putCache("thirdDimension", data.table::data.table(number = 1L, id = "0-5"))
  scenarios <- finalizeScenarios(data.frame(setting = "test"))
  storeScenarios(scenarios, csv = FALSE)
  f <- file.path(getCache("outputsDir"), "v49_1_out.txt")
  writeLines(c("1\t1\t43\t7", "1\t1\t1043\t2", "1\t1\t82\t0.5"), f)

  for (strategy in c("serial", "batch")) {
    collectResults(
      expDir = getCache("experimentDir"), dbDir = rootDir, dbName = strategy,
      replace = TRUE, strategy = strategy
    )
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(rootDir, paste0(strategy, ".sqlite")))
    tryCatch({
      results <- DBI::dbReadTable(con, "results")
      expect_identical(names(results), c(
        "experiment_id", "scenario_id", "survey_date", "third_dimension",
        "measure", "value"
      ))
      expect_identical(results$measure, c(
        "nNewInfections", "nNewInfections_Imported",
        "expectedSevereWithoutComorbidities"
      ))
      expect_equal(results$value, c(7, 2, 0.5))
      expect_identical(results$survey_date, rep("2000-01-01", 3))
      expect_identical(results$third_dimension, rep("0-5", 3))
      schema <- DBI::dbGetQuery(con, "PRAGMA table_info(results)")
      expect_identical(schema$type[schema$name == "measure"], "TEXT")
    }, finally = DBI::dbDisconnect(con))
  }
  clearCache()
})
