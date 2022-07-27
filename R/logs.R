## Helper functions for log file handling


## Load order
##' @include cache.R
NULL

##' @title Remove all files from the log directory
##' @export
deleteLogs <- function() {
  ## Get subdirs of log dir
  logdirs <- list.dirs(getCache(x = "logsDir"))

  ## Get all files for deletion
  logfiles <- c()

  for (d in logdirs) {
    logfiles <- c(list.files(path = d, full.names = TRUE), logfiles)
  }

  ## Delete files
  for (f in logfiles) {
    .printDebug(paste0("Deleting ", f))
    file.remove(f)
  }

  ## Restore directories
  for (d in logdirs) {
    dir.create(d, showWarnings = FALSE)
  }

  return(invisible(TRUE))
}

##' @rdname deleteLogs
##' @export
delete_logs <- deleteLogs

##' @title Clean log directories
##' @param aggregate If TRUE, aggregate all files into a single one.
##' @param compress If TRUE, use create a zip archive per directory.
##' @export
cleanLogs <- function(aggregate = TRUE, compress = TRUE) {
  ## Get only subdirs of log dir
  logdirs <- list.dirs(getCache(x = "logsDir"))

  ## Get all files for deletion
  logfiles <- c()

  ## Aggregate files
  if (aggregate == TRUE) {
    for (d in logdirs) {
      files <- setdiff(
        list.files(d), list.dirs(d, recursive = FALSE, full.names = FALSE)
      )
      if (length(files > 0)) {
        for (f in files) {
          cat(
            paste0(f),
            processFile(file.path(d, f), trim = FALSE, rmdups = FALSE),
            paste(rep("-", options()$width), collapse = ""),
            sep = "\n", file = file.path(d, paste0(basename(d), ".txt")),
            append = TRUE
          )
          file.remove(file.path(d, f))
        }
      }
    }
  }


  ## Compress files
  if (compress == TRUE) {
    for (d in logdirs) {
      ## Work around zip's annoying behavior to include the whole folder
      ## structure in the zip file via changing the workind directory.
      curwd <- getwd()
      setwd(d)
      files <- setdiff(
        list.files(d), list.dirs(d, recursive = FALSE, full.names = FALSE)
      )
      if (length(files > 0)) {
        utils::zip(
          zipfile = basename(d), files = files
        )
      }
      setwd(curwd)
    }

    ## Remove single log files
    file.remove(
      list.files(path = logdirs, pattern = ".*\\.txt$", full.names = TRUE)
    )
  }

  return(invisible(TRUE))
}

##' @rdname cleanLogs
##' @export
clean_logs <- cleanLogs
