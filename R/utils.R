### Small utilities and helper functions which do not fit elsewhere


## Load order
##' @include pkg_setup.R
NULL


##' @title Divide a sequence x into chunks of size n. Any rest is appended.
##' @param x Sequence like 1:10
##' @param n Chunk size
##' @export
splitSeq <- function(x, n) {
  ## Determine number of chunks
  g <- length(x) %/% n
  ## Determine rest, if any
  rest <- length(x) %% n
  ## Sequence of whole groups
  d <- x[1:(n * g)]
  ## Create chunks
  chunks <- split(d, ceiling(seq_along(d) / n))
  ## Add rest, if any
  if (rest != 0) {
    chunks[[paste(length(chunks) + 1)]] <- x[((n * g) + 1):((n * g) + rest)]
  }
  return(chunks)
}

## Adapted from https://stackoverflow.com/a/35761217

##' @title Read file line by line
##' @description Read file line by line and optionally remove whitespace and
##'   duplicate lines. WARNING: Be careful with large files!
##' @param f Path to file.
##' @param trim If TRUE, trim whitespace.
##' @param rmdups If TRUE, only keep lines which are not identical to the
##'   previous line AND which are not whitespace.
##' @return Character vector with each element corresponding to a line.
##' @export
processFile <- function(f, trim = TRUE, rmdups = TRUE) {
  results <- c()
  con <- file(f, "r")
  while (TRUE) {
    line <- readLines(con, n = 1)
    if (trim == TRUE) {
      line <- trimws(line)
    }
    if (length(line) == 0) {
      break
    } else {
      if (rmdups == TRUE) {
        if (length(results) == 0 || line != results[1]) {
          results <- c(line[line != ""], results)
        }
      } else {
        results <- c(line, results)
      }
    }
  }
  close(con)
  return(rev(results))
}

##' @title Build index of a list recursively
##' @param l Input list.
##' @param col Collection list.
##' @param n Current index level.
##' @keywords internal
.indexList <- function(l, col = list(), n = NULL) {
  ## Loop over top level indices
  for (i in seq_along(l)) {
    ## Current entry is not a list
    if (!inherits(l[[i]], what = "list")) {
      ## Build index in the form of x.y.z
      index <- paste0(c(n, i), collapse = ".")
      ## Extract name, value and index information into temporary list
      tmp <- list(a <- list(
        index = c(n, i),
        name = names(l[i]),
        value = l[[i]]
      ))
      ## Assign index as a name for that entry
      names(tmp) <- index
      ## Append to collection
      col <- c(col, tmp)
    } else {
      ## If current entry is a list, only save and append the name of the entry,
      ## like above.
      index <- paste0(c(n, i), collapse = ".")
      tmp <- list(a <- list(
        index = c(n, i),
        name = names(l[i])
      ))
      names(tmp) <- index
      ## Append and recurse into sublist.
      col <- .indexList(l[[i]], col = c(tmp, col), n = c(n, i))
    }
  }
  return(col)
}

##' @title Search for name and/or value
##' @param l Input list.
##' @param name Name to look for.
##' @param value Value to look for.
##' @keywords internal
.getIndex <- function(l, name = NULL, value = NULL) {
  ## First, search for a value if given
  if (!is.null(value)) {
    l <- l[lapply(l, function(x) x[["value"]]) == value]
  }
  ## Second, search for a name if given
  if (!is.null(name)) {
    l <- l[lapply(l, function(x) x[["name"]]) == name]
  }
  return(l)
}

##' @title Extract sublist which matches input
##' @param l Input list.
##' @param name Name to look for.
##' @param value Value to look for.
##' @param onlyIndex If TRUE, only return numeric index of matches.
##' @export
extractList <- function(l, name = NULL, value = NULL, onlyIndex = FALSE) {
  ## Check if at least one of name or value is given
  if (is.null(name) & is.null(value)) {
    stop("At least one of name or value needs to given.")
  }

  ## Build index list
  indexList <- .indexList(l = l)
  ## Search for name and/or value
  indexList <- .getIndex(l = indexList, name = name, value = value)
  ## Check if there are any results
  if (length(indexList) == 0) {
    message("Could not find matches.")
    return(invisible(FALSE))
  } else {
    message(paste0("Found ", length(indexList), " matches."))
  }

  ## Filter candidates. We only want to keep unique entries, which means that if
  ## we have two candidates like 1.2.3 and 1.2.4, the resulting list should be
  ## 1.2 as in contains both hits.
  tmp <- list()
  for (i in seq_along(indexList)) {
    ## Get name of current entry and split along "."
    n <- unlist(strsplit(names(indexList)[i], split = ".", fixed = TRUE))
    ## Remove last element if length > 1
    if (length(n) > 1) {
      n <- n[-length(n)]
    }
    ## Collapse to string
    n <- paste0(n, collapse = ".")
    ## If this name is not present already, copy it into new list
    if (!n %in% names(tmp)) {
      entry <- list(a = indexList[[i]])
      names(entry) <- n
      tmp <- c(tmp, entry)
    }
  }
  ## Overwrite old with new list
  indexList <- tmp

  ## Collect results
  out <- list()

  for (i in seq_along(indexList)) {
    ## If we want to only store the index, do that
    if (onlyIndex == TRUE) {
      index <- indexList[[i]][["index"]]
      entry <- list(a = index)
      names(entry) <- names(indexList[i])
      out <- c(out, entry)
    } else {
      ## Otherwise, collect the list which includes the match into output list.
      index <- indexList[[i]][["index"]]
      entry <- list(
        a = if (length(index) > 1) {
          l[[index[-length(index)]]]
        } else {
          l[[index]]
        }
      )

      names(entry) <- paste0(
        if (length(index) > 1) {
          index[-length(index)]
        } else {
          index
        },
        collapse = "."
      )
      out <- c(out, entry)
    }
  }
  return(out)
}

##' @title Compress files into zip archive
##' @param dir Path to directory.
##' @param pattern An optional regular expression. See \link[base]{list.files}
##' @param value If TRUE, files will be removed after adding them to the
##'   archive.
##' @keywords internal
.compressFiles <- function(dir, pattern = NULL, remove = FALSE) {
  ## Work around zip's annoying behavior to include the whole folder
  ## structure in the zip file via changing the workind directory.
  curwd <- getwd()
  setwd(dir)
  files <- setdiff(
    list.files(dir, pattern = pattern),
    list.dirs(dir, recursive = FALSE, full.names = FALSE)
  )
  if (length(files > 0)) {
    utils::zip(
      zipfile = basename(dir), files = files
    )
  }
  setwd(curwd)
  ## Remove input files
  if (remove == TRUE) {
    file.remove(
      setdiff(
        setdiff(
          list.files(dir, pattern = pattern, full.names = TRUE),
          list.dirs(dir, recursive = FALSE, full.names = TRUE)
        ),
        list.files(path = dir, pattern = ".*\\.zip$", full.names = TRUE)
      )
    )
  }
  return(invisible(TRUE))
}

##' @title Cleanup an experiment
##' @description Cleaning up includes the aggregation and compression of the log
##'   files, as well as the XML files in the scenarios folder and the output
##'   files of Open Malaria.
##' @export
cleanupExperiment <- function() {
  cleanLogs(aggregate = TRUE, compress = TRUE)
  dirs <- c(getCache(x = "outputsDir"), getCache(x = "scenariosDir"))
  for (d in dirs) {
    .compressFiles(dir = d, remove = TRUE)
  }
  return(invisible(TRUE))
}
