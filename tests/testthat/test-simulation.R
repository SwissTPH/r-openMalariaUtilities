test_that("runSimulation throws an error if cmd not found", {
  expect_error(
    runScenarios(scenariosDir = "./"),
    "openMalaria could not be found"
  )
})

test_that("runSimulation throws an error if scenarios do not exist", {
  expect_error(
    ## R should be installed
    runScenarios(scenariosDir = "./", cmd = "R"),
    "does not exist"
  )
})
