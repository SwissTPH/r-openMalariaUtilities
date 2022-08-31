# **NEWS**

# openMalariaUtilities 22.08.0

* First major release of the complete overhaul
  * Open Malaria's results are now collected and stored in a SQLite database
  * The major processing steps, e.g. scenario creation and simulation can now be
    done in parallel. Currently, the collection and storage of the results is
    still single-threaded.
  * Data analysis/post-processing has been removed. It was concluded that this
    is too opionated and out of scope for this package.
