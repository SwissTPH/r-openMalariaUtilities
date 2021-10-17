## TODO Do something useful with these functions, probably when calling
##      openMalaria
is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

is_osx <- function() {
  Sys.info()["sysname"] == "Darwin"
}

## From rmarkdown
## https://github.com/rstudio/rmarkdown/blob/473d80301a0f7b220cf2380e1d5a8199f6971510/R/util.R#L271
## Find a program within the PATH. On OSX we need to explictly call
## /usr/bin/which with a forwarded PATH since OSX Yosemite strips the PATH from
## the environment of child processes
find_program <- function(program) {
  if (is_osx()) {
    res <- suppressWarnings({
      ## Quote the path (so it can contain spaces, etc.) and escape any quotes
      ## and escapes in the path itself
      sanitized_path <- gsub("\\", "\\\\", Sys.getenv("PATH"), fixed = TRUE)
      sanitized_path <- gsub("\"", "\\\"", sanitized_path, fixed = TRUE)
      system(paste0("PATH=\"", sanitized_path, "\" /usr/bin/which ", program),
        intern = TRUE
      )
    })
    if (length(res) == 0) {
      ""
    } else {
      res
    }
  } else {
    Sys.which(program)
  }
}
