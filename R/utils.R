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
