test_that(".createFolders works", {
  omuCache <- hash::hash()

  .createFolders(
    experimentName = "Test",
    rootDir = tempdir(),
    replace = TRUE
  )

  actual <- all.equal(
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    c(
      file.exists(omuCache$baseDir),
      file.exists(omuCache$cacheDir),
      file.exists(omuCache$experimentDir),
      file.exists(omuCache$scenariosDir),
      file.exists(omuCache$logsDir),
      file.exists(omuCache$outputsDir)
    )
  )
  expected <- TRUE
  expect_equal(actual, expected)
})
