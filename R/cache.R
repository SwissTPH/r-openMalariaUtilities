## Caching environment
omuCache <- hash::hash()

.storeCache <- function() {
  save(omuCache,
    file = file.path(omuCache$cacheDir, "cache.RData")
  )
}

.readCache <- function(path) {
  load(file = file.path(path, "cache/cache.RData"), envir = globalenv())
}

##' @title Load cached data from experiment
##' @param path Path of the experiment's folder
##' @export
loadExperiment <- function(path) {
  .readCache(path)
}
