### Caching

## We use the cache to store relevant information of the experiment. This way,
## sessions can be stored on disk and reloaded later. This includes the used
## directories, monitoring time points and the openMalaria input itself.

## A hash table is used for lookup speed.
.pkgcache <- new.env(hash = TRUE, parent = emptyenv())

##' @title Write cache to disk
##' @description Store the created cache to disk, using the 'cacheDir' variable
##'   from the cache itself.
##' @keywords internal
.storeCache <- function() {
  path <- file.path(get("cacheDir", envir = .pkgcache))
  if (!file.exists(path)) {
    dir.create(path)
  }
  saveRDS(.pkgcache,
    file = file.path(path, "cache.rds")
  )
}

##' @title Read cache from disk
##' @description Read cache from 'path'. The folder structure 'cache/cache.rds'
##'   is appended.
##' @param path Path to experiment.
##' @keywords internal
.readCache <- function(path) {
  ## Read into temporary environment
  tempEnv <- readRDS(file = file.path(path, "cache/cache.rds"))
  ## Process each element of tempEnv; assigning it to the original cache.
  for (x in ls(all.names = TRUE, envir = tempEnv)) {
    val <- get(x = paste0(x), envir = tempEnv)
    assign(x = paste0(x), value = val, envir = .pkgcache)
  }
}


##' @title Load cached data from experiment
##' @param path Path of the experiment's folder
##' @export
loadExperiment <- function(path) {
  .readCache(path)
}

##' @title Store object in cache.
##' @param x Object name to store in cache.
##' @param value Object value to store in cache.
##' @export
putCache <- function(x, value) {
  assign(x = as.character(x), value = value, envir = .pkgcache)
}

##' @title Get cached object from cache.
##' @param x Object name to retrieve from cache.
##' @export
getCache <- function(x) {
  get(x = as.character(x), envir = .pkgcache)
}

##' @title Remove all objects from cache.
##' @export
clearCache <- function() {
  rm(
    list = ls(all.names = TRUE, envir =.pkgcache),
    envir = .pkgcache
  )
}

##' @title List objects in cache.
##' @export
listCache <- function() {
  return(ls(all.names = TRUE, envir = .pkgcache))
}
