## Toggleable verbose and debug printing.
## Instead of spamming the user with status messages, we implement toggleable
## printing funcitons which can be used in functions. Thus, the output is
## available only when needed.


##' @include pkg_setup.R
NULL


assign("verboseOutput", FALSE, envir = .pkgenv)
assign("debugOutput", FALSE, envir = .pkgenv)


##' @title Toggle verbose messages
##' @description Enables the output of verbose messages in functions which have
##'   that implemented. Verbose output is off by default.
##' @export
toggleVerbose <- function() {
  curVal <- get("verboseOutput", envir = .pkgenv)
  if (curVal == FALSE) {
    assign("verboseOutput", TRUE, envir = .pkgenv)
  } else {
    assign("verboseOutput", FALSE, envir = .pkgenv)
  }
}

##' @title Toggle debug messages
##' @description Enables the output of debug messages in functions which have
##'   that implemented. Debug output is off by default.
##' @export
toggleDebug <- function() {
  curVal <- get("debugOutput", envir = .pkgenv)
  if (curVal == FALSE) {
    assign("debugOutput", TRUE, envir = .pkgenv)
    assign("verboseOutput", TRUE, envir = .pkgenv)
  } else {
    assign("debugOutput", FALSE, envir = .pkgenv)
    assign("verboseOutput", FALSE, envir = .pkgenv)
  }
}

##' @title Print verbose messages
##' @description Like print, but only if 'verboseOutput' is TRUE.
##' @param x Object to print.
##' @param ... Further arguments to [print()].
##' @keywords internal
.printVerbose <- function(x, ...) {
  curVal <- get("verboseOutput", envir = .pkgenv)
  if (curVal == TRUE) {
    cat(paste0("INFO\t", utils::capture.output(print(x, ...))), sep = "\n")
  }
}

##' @title Print debug messages
##' @description Like print, but only if 'debugOutput' is TRUE.
##' @param x Object to print.
##' @param ... Further arguments to [print()].
##' @keywords internal
.printDebug <- function(x, ...) {
  curVal <- get("debugOutput", envir = .pkgenv)
  if (curVal == TRUE) {
    cat(paste0("DEBUG\t", utils::capture.output(print(x, ...))), sep = "\n")
  }
}
