![Latest commit](https://img.shields.io/github/last-commit/SwissTPH/r-openMalariaUtilities/master?style=flat-square)
![Build status: master](https://img.shields.io/github/workflow/status/SwissTPH/r-openMalariaUtilities/R-CMD-check/master?style=flat-square)
![coverage](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/rogoersTPH/db82500941e9bea7d7a5c0bfd5e6db8d/raw/cover.json)


# openMalariaUtilities

Helper functions to provide easier usage of [Open
Malaria](https://github.com/SwissTPH/openmalaria) from R

# Install instructions

A CRAN release is currently not available. You can install either a tagged
version via

``` R
devtools::install_github("SwissTPH/r-openMalariaUtilities", ref = TAG)
```

Replace `TAG` with the tag you want to use.

If you want to use the latest development version on `master`, please use

``` R
devtools::install_github("SwissTPH/r-openMalariaUtilities")
```

`V2-stable` is the historical branch and can be installed via

``` R
devtools::install_github("SwissTPH/r-openMalariaUtilities", ref = "V2-stable")
```

# Dependencies

In order for this package to work, please make sure that you have correctly
installed [Open Malaria](https://github.com/SwissTPH/openmalaria/wiki/UserGuide)
and SQLite 3 on your system.
