### Caching

## We use the cache to store relevant information of the experiment. This way,
## sessions can be stored on disk and reloaded later. This includes the used
## directories, monitoring time points and the openMalaria input itself.

## A hash table is used for lookup speed.
.omupkgcache <- new.env(hash = TRUE, parent = emptyenv())

.storeCache <- function(env = parent.frame()) {
  if (!file.exists(file.path(.omupkgcache$cacheDir))) {
    dir.create(file.path(.omupkgcache$cacheDir))
  }
  saveRDS(.omupkgcache,
    file = file.path(.omupkgcache$cacheDir, "cache.rds")
  )
}

.readCache <- function(path) {
  .omupkgcache <<- readRDS(file = file.path(path, "cache/cache.rds"))
}

##' @title Load cached data from experiment
##' @param path Path of the experiment's folder
##' @export
loadExperiment <- function(path) {
  .readCache(path)
}
