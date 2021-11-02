### Creation of the directory structure.

## If not defined otherwise, the current working directory is taken as the root
## of the structure. A subfolder named after the experiment name (if given) or
## with the current date and time is created in the root folder and contains
## distinct folders for the cache, the logs, the scenarions and the results.

## NOTE Directory structure is created as a side effect

##' Generate the folder structure for an experiment. Caches the paths in the
##' `.openMalariaUtilites` environment.
##'
##' @title Create folder structure
##' @param experimentName Name of the experiment. Defaults to current timestamp
##' @param rootDir Root directory as a string. Defaults to the current working
##'   directory.
##' @param scenariosDir Scenarios directory name as a string. Will be a
##'   subfolder of the `rootDir`. Defaults to 'scenarios'.
##' @param logsDir Logs directory name as a string. Will be a subfolder of the
##'   `rootDir`. Defaults to 'logs'.
##' @param replace If the present directory structure should be replaced. Can be
##'   TRUE, FALSE or "ask".
.createFolders <- function(experimentName = NULL,
                           rootDir = NULL,
                           scenariosDir = NULL,
                           logsDir = NULL,
                           replace = "ask",
                           env = parent.frame()) {
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

  omuCache <- (get("omuCache", envir = env))

  ## Generate paths and cache them
  ## Unless rootDir is given, use current working directory
  if (is.null(rootDir)) {
    rootDir <- getwd()
  }
  omuCache$baseDir <- file.path(rootDir)

  ## Experiment directory
  if (is.null(experimentName)) {
    experimentName <- format(Sys.time(), "%Y%m%d_%H%M%S")
  }
  omuCache$experimentDir <- file.path(rootDir, experimentName)

  ## Cache directory
  omuCache$cacheDir <- file.path(omuCache$experimentDir, "cache")

  ## Scenario directory
  if (is.null(scenariosDir)) {
    scenariosDir <- "scenarios"
  }

  omuCache$scenariosDir <- file.path(
    rootDir,
    experimentName,
    scenariosDir
  )

  ## Logs directory
  if (is.null(logsDir)) {
    logsDir <- "logs"
  }

  omuCache$logsDir <- file.path(
    rootDir,
    experimentName,
    logsDir
  )

  ## Output directory
  omuCache$outputsDir <- file.path(
    rootDir,
    experimentName,
    "outputs"
  )

  ## Check if directories are already present and crete them if necessary
  createDir <- NULL
  if (dir.exists(omuCache$experimentDir)) {
    if (replace == FALSE) {
      stop("Directory with experiment name already present. Aborting.")
    } else if (replace == "ask") {
      answer <- utils::askYesNo("Directory with experiment name already present. Replace?")
      if (!answer == TRUE | is.na(answer)) {
        stop("Aborting.")
      }
    } else {
      createDir <- TRUE
      unlink(omuCache$experimentDir, recursive = TRUE)
    }
  } else {
    createDir <- TRUE
  }

  ## Create directories, silently
  if (createDir == TRUE) {
    invisible(
      lapply(
        c(
          omuCache$baseDir,
          omuCache$cacheDir,
          omuCache$experimentDir,
          omuCache$scenariosDir,
          omuCache$logsDir,
          omuCache$outputsDir
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
