test_that(".createFolders works", {
  .createFolders(
    experimentName = "Test",
    rootDir = tempdir(),
    replace = TRUE
  )

  actual <- all.equal(
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    c(
      file.exists(get(x = "baseDir", envir = openMalariaUtilities:::.pkgcache)),
      file.exists(get(x = "cacheDir", envir = openMalariaUtilities:::.pkgcache)),
      file.exists(get(x = "experimentDir", envir = openMalariaUtilities:::.pkgcache)),
      file.exists(get(x = "scenariosDir", envir = openMalariaUtilities:::.pkgcache)),
      file.exists(get(x = "logsDir", envir = openMalariaUtilities:::.pkgcache)),
      file.exists(get(x = "outputsDir", envir = openMalariaUtilities:::.pkgcache))
    )
  )
  expected <- TRUE
  expect_equal(actual, expected)
})
