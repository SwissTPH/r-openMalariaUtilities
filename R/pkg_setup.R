## This file contains the basic package setup, including any environments which
## are used to store global variables.
## In particular, these are used for the printing helpers and the cache.

## Load order matters. Make sure to use
## ##' @include pkg_setup.R
## NULL
## when necessary

## Cache environment
.pkgcache <- new.env(hash = TRUE, parent = emptyenv())

## General environment
.pkgenv <- new.env(hash = TRUE, parent = emptyenv())
