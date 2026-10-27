# **NEWS**

# Development version

* Correct the `defineEntomology()` help example to use a three-day resting
  period and a 0.33-day host-seeking period, with comments explaining the units.
  The builder's field mapping and existing scenarios are unchanged.
* Fix vector conditions in `defineEntomology()` for R >= 4.3. Monthly and daily
  placeholder vectors are accepted only when every value matches the placeholder
  pattern; numeric daily seasonality is also handled correctly.
* Add the OpenMalaria v49 schema and setup support, retaining v44 as the default.
* Make `defineEntomology()` and `defineDemography()` version-aware while
  preserving v44 behaviour. V49 omits `propInfected` and `propInfectious` and
  rejects non-NULL `growthRate` input.
* Adapt `write_end_compat()` for v49 seed placement, explicit latent-period
  units, and removal of obsolete parameter attributes. Preserve legacy model
  parameters and clinical-memory behaviour. Other compatibility helpers are
  unchanged.
* Add version-aware output translation, including v49 infection-origin and
  clinical measures. Preserve the existing v44 dictionary and labels. Unknown
  measure IDs now warn and remain identifiable instead of becoming NA.
* Add a v49 workflow and migration vignette, plus v44/v49 integration CI using
  pinned executables, a frozen v44 input reference, and serial/batch SQLite
  checks. Local paired runs pass; hosted CI still needs confirmation after push.


# openMalariaUtilities 23.02

Feature release

* Breaking
  * `readResults` is superseded by `collectResults`
    ([1ec31c9](https://github.com/SwissTPH/r-openMalariaUtilities/commit/1ec31c9260f9cfa1e9bdd3861eb804ee27e9f36a))
    * Now, the collection of the results can be parallelized by two strategies
    * The data to be stored in the SQLite database can be customized by using
      three function which can be passed by arguments
  * `generateScenarios` has been deprecated and replaced by `finalizeScenarios`

* Various bug fixes and improvements

# openMalariaUtilities 22.08.1

Maintenance release

* Fixes
  * Allow the use of no interventions ([d504923](https://github.com/SwissTPH/r-openMalariaUtilities/commit/d504923fa047fc56a5047539e523a19abb9d20f1))
  * Fix unexpected behavior in splitSeq ([cd6c0f1](https://github.com/SwissTPH/r-openMalariaUtilities/commit/cd6c0f19c6d9d2b45960f0cecd87695d662fd8e7))
  * Fix undefined placeholder detection ([4dbf142](https://github.com/SwissTPH/r-openMalariaUtilities/commit/4dbf142b121c0f3a1fac989e082cf46673a83cbd))

* Improvements
  * Warn if simStart and startDate are not the same ([08f6ec5](https://github.com/SwissTPH/r-openMalariaUtilities/commit/08f6ec5b149d48cd43b9d02e881b79b0c98e41ae))
  * Add `deploy_cont_compat` ([6f8b483](https://github.com/SwissTPH/r-openMalariaUtilities/commit/6f8b483fc478b380580b082a48fa8eca087c1049),
    [4ec4189](https://github.com/SwissTPH/r-openMalariaUtilities/commit/4ec418936ddbce5c343901b0816036f6e04423ec), [38d6345](https://github.com/SwissTPH/r-openMalariaUtilities/commit/38d634541e55ce0b17d079b50452de4445dd9256))
  * Add experiment summary ([1334d9c](https://github.com/SwissTPH/r-openMalariaUtilities/commit/1334d9ceb2b91cb4ab270b12d6d17b98ce1105c7))

# openMalariaUtilities 22.08.0

* First major release of the complete overhaul
  * Open Malaria's results are now collected and stored in a SQLite database
  * The major processing steps, e.g. scenario creation and simulation can now be
    done in parallel. Currently, the collection and storage of the results is
    still single-threaded.
  * Data analysis/post-processing has been removed. It was concluded that this
    is too opionated and out of scope for this package.
