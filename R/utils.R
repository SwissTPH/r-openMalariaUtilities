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
