test_that(".createFolders works", {
  .createFolders(
    experimentName = "Test",
    rootDir = tempdir(),
    replace = TRUE
  )

  actual <- all.equal(
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    c(
      file.exists(.omupkgcache$baseDir),
      file.exists(.omupkgcache$cacheDir),
      file.exists(.omupkgcache$experimentDir),
      file.exists(.omupkgcache$scenariosDir),
      file.exists(.omupkgcache$logsDir),
      file.exists(.omupkgcache$outputsDir)
    )
  )
  expected <- TRUE
  expect_equal(actual, expected)
})
