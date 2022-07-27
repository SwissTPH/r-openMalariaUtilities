### Creation of the directory structure.

## If not defined otherwise, the current working directory is taken as the root
## of the structure. A subfolder named after the experiment name (if given) or
## with the current date and time is created in the root folder and contains
## distinct folders for the cache, the logs, the scenarions and the results.

## NOTE Directory structure is created as a side effect

##' @include cache.R printing.R
NULL

##' @title Create a folder and return the path
##' @description If a folder does not exist, create it and return the path.
##'   Otherwise, just return the path.
##' @param path Path of directory.
##' @keywords internal
.useDir <- function(path) {
  if (!dir.exists(paths = path)) {
    .printDebug(paste0("Directory ", path, " does not exist, creating ..."))
    dir.create(
      path = path, recursive = TRUE,
      showWarnings = get("debugOutput", envir = .pkgenv)
    )
  }
  return(path)
}

##' @title Create folder structure
##' @description Generate the folder structure for an experiment. Caches the
##'   paths in the `.pkgcache` environment.
##' @param experimentName Name of the experiment. Spaces will be replaced by
##'   underscores.
##' @param rootDir Root directory as a string. Defaults to the current working
##'   directory.
##' @param scenariosDir Scenarios directory name as a string. Will be a
##'   subfolder of the `rootDir`. Defaults to 'scenarios'.
##' @param logsDir Logs directory name as a string. Will be a subfolder of the
##'   `rootDir`. Defaults to 'logs'.
##' @param replace If the present directory structure should be replaced. Can be
##'   TRUE, FALSE or "ask".
##' @keywords internal
.createFolders <- function(experimentName, rootDir = NULL, scenariosDir = NULL,
                           logsDir = NULL, replace = "ask") {
  ## Input verification
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(experimentName, add = assertCol)
  checkmate::assert(
    checkmate::checkCharacter(rootDir, null.ok = TRUE),
    add = assertCol
  )
  checkmate::assert(
    checkmate::checkCharacter(scenariosDir, null.ok = TRUE),
    add = assertCol
  )
  checkmate::assert(
    checkmate::checkCharacter(logsDir, null.ok = TRUE),
    add = assertCol
  )
  checkmate::assert(
    checkmate::checkLogical(replace),
    checkmate::checkCharacter(replace, pattern = "ask"),
    add = assertCol
  )
  checkmate::reportAssertions(collection = assertCol)

  ## Generate paths and cache them
  ## Unless rootDir is given, use current working directory
  if (is.null(rootDir)) {
    rootDir <- getwd()
  }
  putCache(x = "rootDir", value = file.path(rootDir))

  ## Replace spaces with underscores in experiment name and cache it
  experimentName <- gsub(" ", "_", experimentName)
  putCache(x = "experimentName", value = experimentName)

  ## Experiment directory
  putCache(x = "experimentDir", value = file.path(rootDir, experimentName))

  ## Cache directory
  putCache(
    x = "cacheDir", value = file.path(getCache(x = "experimentDir"), "cache")
  )

  ## Scenario directory
  if (is.null(scenariosDir)) {
    scenariosDir <- "scenarios"
  }

  putCache(
    x = "scenariosDir", value = file.path(rootDir, experimentName, scenariosDir)
  )


  ## Logs directory
  if (is.null(logsDir)) {
    logsDir <- "logs"
  }

  putCache(x = "logsDir", value = file.path(rootDir, experimentName, logsDir))

  ## Output directory
  putCache(
    x = "outputsDir", value = file.path(rootDir, experimentName, "outputs")
  )

  ## Check if directories are already present and crete them if necessary
  createDir <- NULL
  if (dir.exists(getCache(x = "experimentDir"))) {
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
        unlink(getCache(x = "experimentDir"), recursive = TRUE)
      }
      ## Directory present, replace
    } else {
      createDir <- TRUE
      unlink(getCache(x = "experimentDir"), recursive = TRUE)
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
          getCache(x = "rootDir"),
          ## Experiment directory
          getCache(x = "experimentDir"),
          ## Cache directory
          getCache(x = "cacheDir"),
          ## Scenarios directory
          getCache(x = "scenariosDir"),
          ## Log directory
          getCache(x = "logsDir"),
          file.path(getCache(x = "logsDir"), "scenarios"),
          file.path(getCache(x = "logsDir"), "simulation"),
          ## Open Malaria output directory
          getCache(x = "outputsDir")
        ),
        function(x) {
          if (!dir.exists(x)) {
            .printVerbose(paste0("Creating directory ", x))
            dir.create(x, showWarnings = TRUE, recursive = TRUE)
          }
        }
      )
    )
  }
  ## Write cache
  .synchronizeCache(direction = "none")
}

##' @title Create folder structure
##' @description Generate the folder structure for an experiment.
##' @param experimentName Name of the experiment. Spaces will be replaced by
##'   underscores.
##' @param rootDir Root directory as a string. Defaults to the current working
##'   directory.
##' @param scenariosDir Scenarios directory name as a string. Will be a
##'   subfolder of the `rootDir`. Defaults to 'scenarios'.
##' @param logsDir Logs directory name as a string. Will be a subfolder of the
##'   `rootDir`. Defaults to 'logs'.
##' @param replace If the present directory structure should be replaced. Can be
##'   TRUE, FALSE or "ask". This will remove any existing content!
##' @export
setupDirs <- function(experimentName, rootDir = NULL, scenariosDir = NULL,
                      logsDir = NULL, replace = "ask") {
  .createFolders(
    experimentName = experimentName, rootDir = rootDir,
    scenariosDir = scenariosDir, logsDir = logsDir, replace = replace
  )
}

##' @rdname setupDirs
##' @export
setup_dirs <- setupDirs
