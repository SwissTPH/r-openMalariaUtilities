![Latest commit](https://img.shields.io/github/last-commit/SwissTPH/r-openMalariaUtilities/master?style=flat-square)
![Build status: master](https://img.shields.io/github/actions/workflow/status/SwissTPH/r-openMalariaUtilities/check.yml?branch=master&style=flat-square)
![coverage](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/rogoersTPH/db82500941e9bea7d7a5c0bfd5e6db8d/raw/cover.json)


# openMalariaUtilities

Helper functions to provide easier usage of
[OpenMalaria](https://github.com/SwissTPH/openmalaria) from R

# Install instructions

A CRAN release is currently not available. You can install either a tagged
version via

``` R
devtools::install_github("SwissTPH/r-openMalariaUtilities", ref = "TAG")
```

Replace `TAG` with the tag you want to use, e.g. "22.10".

If you want to use the latest development version from `master`, please use

``` R
devtools::install_github("SwissTPH/r-openMalariaUtilities")
```

`V2-stable` is the historical branch and can be installed via

``` R
devtools::install_github("SwissTPH/r-openMalariaUtilities", ref = "V2-stable")
```

# Dependencies

In order for this package to work, please make sure that you have correctly
installed [OpenMalaria](https://github.com/SwissTPH/openmalaria/wiki/UserGuide)
and SQLite 3 on your system.

# OpenMalaria versions

The version-aware XML builders and output reader support OpenMalaria 44 and 49.
`setupOM()` still defaults to 44. For v49, set `OMVersion = 49L` in the scenario
list before calling builders, use `setupOM(version = 49)`, and run the generated
scenarios with an OpenMalaria v49 executable.

The [v49 workflow and migration guide](vignettes/openmalaria-v49.Rmd) provides
a complete example, schema-validation steps, and the modelling assumptions
preserved during migration. Existing introductory examples remain v44 examples.
Only `write_end_compat()` has been updated for v49; other `*_compat` helpers
remain unchanged and are outside the supported v49 workflow.

Output translation follows the experiment's cached `OMVersion`, with an explicit
`version` override available in `readOutputFile()`. Existing v44 labels remain
unchanged. V49 uses the corrected `nNewInfections` label, and unknown measure IDs
now generate a warning and are retained as text. Paired execution and CI checks
with both OpenMalaria versions remain a separate validation stage.
