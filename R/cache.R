### Caching

## We use the cache to store relevant information of the experiment. This way,
## sessions can be stored on disk and reloaded later. This includes the used
## directories, monitoring time points and the openMalaria input itself.

## A hash table is used for lookup speed.

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
