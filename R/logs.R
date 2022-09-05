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
  ## Input verification
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertLogical(aggregate, add = assertCol)
  checkmate::assertLogical(compress, add = assertCol)
  checkmate::reportAssertions(assertCol)

  ## Get only subdirs of log dir
  logdirs <- list.dirs(getCache(x = "logsDir"))

  ## Aggregate files
  ##
  ## NOTE Using parallel's cluster here was investigated and turned out to be
  ##      slower overall. The idea was to collect the raw output files into
  ##      temporary files in batches of 100 to 200 files each. Finally, these
  ##      temporary files would be added to the destination file. The first step
  ##      was really fast but the last step much slower then expected.
  if (aggregate == TRUE) {
    for (d in logdirs) {
      files <- setdiff(
        list.files(d), list.dirs(d, recursive = FALSE, full.names = FALSE)
      )
      if (length(files > 0)) {
        for (f in files) {
          cat(
            paste0(f),
            data.table::fread(file.path(d, f), sep = "\n", header = FALSE)[[1]],
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
      .compressFiles(dir = d, pattern = ".*\\.txt$", remove = TRUE)
    }
  }

  return(invisible(TRUE))
}

##' @rdname cleanLogs
##' @export
clean_logs <- cleanLogs
