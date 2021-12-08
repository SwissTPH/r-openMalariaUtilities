### Caching

## We use the cache to store relevant information of the experiment. This way,
## sessions can be stored on disk and reloaded later. This includes the used
## directories, monitoring time points and the openMalaria input itself.

## A hash table is used for lookup speed.
.pkgcache <- new.env(hash = TRUE, parent = emptyenv())

.storeCache <- function() {
  path <- file.path(get("cacheDir", envir = .pkgcache))
  if (!file.exists(path)) {
    dir.create(path)
  }
  saveRDS(.pkgcache,
    file = file.path(path, "cache.rds")
  )
}

.readCache <- function(path) {
  tempEnv <- readRDS(file = file.path(path, "cache/cache.rds"))
  invisible(
    sapply(ls(all.names = TRUE, envir = tempEnv), function(x) {
      val <- get(x = paste0(x), envir = tempEnv)
      assign(x = paste0(x), value = val, envir = .pkgcache)
    })
  )
}

##' @title Load cached data from experiment
##' @param path Path of the experiment's folder
##' @export
loadExperiment <- function(path) {
  .readCache(path)
}
