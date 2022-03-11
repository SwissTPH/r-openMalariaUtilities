### Creation of the directory structure.

## If not defined otherwise, the current working directory is taken as the root
## of the structure. A subfolder named after the experiment name (if given) or
## with the current date and time is created in the root folder and contains
## distinct folders for the cache, the logs, the scenarions and the results.

## NOTE Directory structure is created as a side effect

##' @title Create folder structure
##' @description Generate the folder structure for an experiment. Caches the
##'   paths in the `.openMalariaUtilites` environment.
##' @param experimentName Name of the experiment. Defaults to current timestamp
##' @param rootDir Root directory as a string. Defaults to the current working
##'   directory.
##' @param scenariosDir Scenarios directory name as a string. Will be a
##'   subfolder of the `rootDir`. Defaults to 'scenarios'.
##' @param logsDir Logs directory name as a string. Will be a subfolder of the
##'   `rootDir`. Defaults to 'logs'.
##' @param replace If the present directory structure should be replaced. Can be
##'   TRUE, FALSE or "ask".
##' @keywords internal
.createFolders <- function(experimentName = NULL,
                           rootDir = NULL,
                           scenariosDir = NULL,
                           logsDir = NULL,
                           replace = "ask") {
  ## Input verification
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(experimentName, add = assertCol)
  checkmate::assert(checkmate::checkNull(rootDir),
    checkmate::checkCharacter(rootDir),
    add = assertCol
  )
  checkmate::assert(checkmate::checkNull(scenariosDir),
    checkmate::checkCharacter(scenariosDir),
    add = assertCol
  )
  checkmate::assert(checkmate::checkNull(logsDir),
    checkmate::checkCharacter(logsDir),
    add = assertCol
  )
  checkmate::assert(checkmate::checkLogical(replace),
    checkmate::checkCharacter(replace, pattern = "ask"),
    add = assertCol
  )
  checkmate::reportAssertions(collection = assertCol)

  ## Generate paths and cache them
  ## Unless rootDir is given, use current working directory
  if (is.null(rootDir)) {
    rootDir <- getwd()
  }
  assign(x = "baseDir", value = file.path(rootDir), envir = .pkgcache)


  ## Experiment directory
  if (is.null(experimentName)) {
    experimentName <- format(Sys.time(), "%Y%m%d_%H%M%S")
  }
  assign(
    x = "experimentDir", value = file.path(rootDir, experimentName),
    envir = .pkgcache
  )

  ## Cache directory
  assign(x = "cacheDir", value = file.path(get(
    x = "experimentDir",
    envir = .pkgcache
  ), "cache"), envir = .pkgcache)

  ## Scenario directory
  if (is.null(scenariosDir)) {
    scenariosDir <- "scenarios"
  }

  assign(x = "scenariosDir", value = file.path(
    rootDir,
    experimentName,
    scenariosDir
  ), envir = .pkgcache)


  ## Logs directory
  if (is.null(logsDir)) {
    logsDir <- "logs"
  }

  assign(x = "logsDir", value = file.path(
    rootDir,
    experimentName,
    logsDir
  ), envir = .pkgcache)

  ## Output directory
  assign(x = "outputsDir", value = file.path(
    rootDir,
    experimentName,
    "outputs"
  ), envir = .pkgcache)

  ## Combined outputs directory
  assign(x = "combinedDir", value = file.path(
    rootDir,
    experimentName,
    "combined"
  ), envir = .pkgcache)

  ## Check if directories are already present and crete them if necessary
  createDir <- NULL
  if (dir.exists(get(x = "experimentDir", envir = .pkgcache))) {
    ## Directory present, no replace
    if (replace == FALSE) {
      stop("Directory with experiment name already present. Aborting.")
      ## Directory present, ask
    } else if (replace == "ask") {
      answer <- utils::askYesNo("Directory with experiment name already present. Replace?")
      ## No or no answer
      if (!answer == TRUE | is.na(answer)) {
        stop("Aborting.")
        ## Yes
      } else {
        createDir <- TRUE
        unlink(get(x = "experimentDir", envir = .pkgcache), recursive = TRUE)
      }
      ## Directory present, replace
    } else {
      createDir <- TRUE
    }
    ## Directory not present
  } else {
    createDir <- TRUE
  }

  ## Create directories, silently
  if (createDir == TRUE) {
    invisible(
      lapply(
        c(
          ## Project root
          get(x = "baseDir", envir = .pkgcache),
          ## Experiment directory
          get(x = "experimentDir", envir = .pkgcache),
          ## Cache directory
          get(x = "cacheDir", envir = .pkgcache),
          ## Scenarios directory
          get(x = "scenariosDir", envir = .pkgcache),
          ## Log directory
          get(x = "logsDir", envir = .pkgcache),
          file.path(get(x = "logsDir", envir = .pkgcache), "scenarios"),
          file.path(get(x = "logsDir", envir = .pkgcache), "simulation"),
          file.path(get(x = "logsDir", envir = .pkgcache), "postprocessing"),
          ## Open Malaria output directory
          get(x = "outputsDir", envir = .pkgcache),
          ## Combined outputs directory
          get(x = "combinedDir", envir = .pkgcache)
        ),
        function(x) {
          if (!dir.exists(x)) {
            dir.create(x, showWarnings = TRUE, recursive = TRUE)
          }
        }
      )
    )
  }
}
