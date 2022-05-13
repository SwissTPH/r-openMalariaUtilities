### Caching

## We use the cache to store relevant information of the experiment. This way,
## sessions can be stored on disk and reloaded later. This includes the used
## directories, monitoring time points and the openMalaria input itself.

## Most of the functions here are used for their side effects.

##' @include pkg_setup.R
NULL

##' @title Writes .pkgcache to disk
##' @param path Path to 'cache.rds' file.
##' @keywords internal
.syncDisk <- function(path) {
  saveRDS(.pkgcache, file = file.path(path, "cache.rds"))
  return(invisible(TRUE))
}

##' @title Assign values from diskCache to .pkgcache
##' @param diskCache Cache environment
##' @keywords internal
.syncMem <- function(diskCache) {
  if (length(diskCache) == 0) {
    stop("Disk cache not found or empty. Aborting.")
  }
  clearCache()
  for (x in ls(all.names = TRUE, envir = diskCache)) {
    .printDebug(paste0("Retrieving ", as.character(x), "from cache."))
    val <- get(x = as.character(x), envir = diskCache)
    .printDebug(paste0("Values: ", val))
    assign(x = as.character(x), value = val, envir = .pkgcache)
  }
  return(invisible(TRUE))
}

##' @title Synchronize diskCache and memCache
##' @param diskCache Cache environment
##' @param memCache Cache environment
##' @keywords internal
.syncBoth <- function(diskCache, memCache) {
  allObj <- union(
    ls(all.names = TRUE, envir = diskCache),
    ls(all.names = TRUE, envir = memCache)
  )
  .printDebug(cat("Found objects to sync: ", allObj, sep = "\n"))
  tempCache <- new.env(hash = TRUE, parent = emptyenv())

  ## Loop over all objects
  for (el in allObj) {
    ## Check if element exists both caches
    mem <- if (exists(as.character(el), envir = memCache)) {
      get(x = as.character(el), envir = memCache)
    } else {
      NULL
    }
    disk <- if (exists(as.character(el), envir = diskCache)) {
      get(x = as.character(el), envir = diskCache)
    } else {
      NULL
    }

    ## If one element is NA, simply assign the other
    if (is.null(mem) | is.null(disk)) {
      if (is.null(mem)) {
        .printDebug(paste0("Memory empty, using disk value for ", el))
        assign(x = as.character(el), value = disk, envir = tempCache)
      } else {
        .printDebug(paste0("Disk cache empty, using memory value for ", el))
        assign(x = as.character(el), value = mem, envir = tempCache)
      }
    } else {
      ## Otherwise take the one with the later timestamp
      memTs <- mem[["timestamp"]]
      diskTs <- disk[["timestamp"]]
      if (memTs >= diskTs) {
        .printDebug(paste0("Timestamp for ", el, " newer in memory. ", memTs))
        assign(x = as.character(el), value = mem, envir = tempCache)
      } else {
        .printDebug(paste0("Timestamp for ", el, " newer on disk. ", diskTs))
        assign(x = as.character(el), value = disk, envir = tempCache)
      }
    }
  }
  return(tempCache)
}

##' @title Synchronize the Cache
##' @param path Path to cache directory.
##' @param direction Directions of sync. Can be one of "disk" = memory cache
##'   overrides disk cache, "memory" = disk cache overrides memory cache and
##'   "none" = caches get synchronized via timestamps.
##' @keywords internal
.synchronizeCache <- function(path = NULL,
                              direction = c("disk", "memory", "none")) {
  ## Input verification
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertSubset(
    direction,
    choices = c("disk", "memory", "none"),
    add = assertCol
  )
  checkmate::reportAssertions(assertCol)

  if (is.null(path)) {
    path <- file.path(getCache("cacheDir"))
  }
  ## Make sure cache directory is available
  dir.create(path, showWarnings = get("debugOutput", envir = .pkgenv))

  ## If direction is "disk", simply write the cache to disk and be done.
  if (direction == "disk") {
    .printVerbose("Writing cache to disk ...")
    .syncDisk(path = path)
    return(invisible(TRUE))
  } else {
    ## Else read the disk cache into temporary environment, we are going to need
    ## it. If the disk cache does not exist, use an empty environment.
    if (file.exists(file.path(path, "cache.rds"))) {
      diskCache <- readRDS(file = file.path(path, "cache.rds"))
    } else {
      diskCache <- new.env(hash = TRUE, parent = emptyenv())
    }
  }

  ## If direction is "memory", clear cache in memory and assign values from
  ## diskCache
  if (direction == "memory") {
    .printVerbose("Retrieving cache from disk ...")
    .syncMem(diskCache = diskCache)
    return(invisible(TRUE))
  } else {
    .printVerbose("Synchronizing disk and memory cache ...")
    ## Otherwise we need to sync the elements of both caches
    tempCache <- .syncBoth(diskCache = diskCache, memCache = .pkgcache)
    ## Assign the new cache both to memory and to disk
    .printVerbose("Assigning values to memory cache ...")
    for (x in ls(all.names = TRUE, envir = tempCache)) {
      val <- get(x = as.character(x), envir = tempCache)
      assign(x = as.character(x), value = val, envir = .pkgcache)
    }
    .printVerbose("Writing disk cache ...")
    saveRDS(tempCache, file = file.path(path, "cache.rds"))
    return(invisible(TRUE))
  }
}

##' @title Load cached data from experiment
##' @param path Path of the experiment's folder
##' @export
loadExperiment <- function(path) {
  .synchronizeCache(path = file.path(path, "cache"), direction = "memory")
}

##' @rdname loadExperiment
##' @export
load_experiment <- loadExperiment

##' @title Store object in cache.
##' @param x Object name to store in cache.
##' @param value Object value to store in cache.
##' @export
putCache <- function(x, value) {
  ts <- Sys.time()
  value <- list(value = value, timestamp = ts)
  assign(x = as.character(x), value = value, envir = .pkgcache)
}

##' @rdname putCache
##' @export
put_cache <- putCache

##' @title Get cached object from cache.
##' @param x Object name to retrieve from cache.
##' @param ret What values to return. Can be one of "value", "timestamp" or
##'   "both".
##' @export
getCache <- function(x, ret = "value") {
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertSubset(
    ret,
    choices = c("value", "timestamp", "both"),
    add = assertCol
  )
  checkmate::reportAssertions(assertCol)

  values <- get(x = as.character(x), envir = .pkgcache)
  val <- values[["value"]]
  ts <- values[["timestamp"]]
  if (ret == "timestamp") {
    return(ts)
  } else if (ret == "value") {
    return(val)
  } else {
    return(values)
  }
}

##' @rdname getCache
##' @export
get_cache <- getCache

##' @title Remove all objects from cache.
##' @export
clearCache <- function() {
  rm(
    list = ls(all.names = TRUE, envir = .pkgcache),
    envir = .pkgcache
  )
}

##' @rdname clearCache
##' @export
clear_cache <- clearCache

##' @title List objects and values in cache.
##' @export
listCache <- function() {
  objs <- ls(all.names = TRUE, envir = .pkgcache)
  out <- list()
  for (obj in objs) {
    tmp <- getCache(obj, ret = "both")
    cat("
Name:", obj, "\tTimestamp:", format(tmp[["timestamp"]], format = "%Y-%m-%d %H:%M:%S"), "
Class:", class(tmp[["value"]]), "
Value:
", paste(capture.output(tmp[["value"]]), "\n", sep = ""), "
", paste(rep("-", options()$width), collapse = ""), "
")
  }
}

##' @rdname listCache
##' @export
list_cache <- listCache
