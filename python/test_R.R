library(R6)
library(reticulate)

OMExperiment <- R6Class(
  "OMExperiment",
  public = list(
    xml_tree = NULL,
    scenario = NULL,
    initialize = function(om_version) {
      ## Import etree from lxml
      etree <- import("lxml.etree")
      ## Create XML string
      xml_str <- paste0(
        '<?xml version="1.0" encoding="UTF-8" standalone="no"?>',
        '<om:scenario xmlns:om="http://openmalaria.org/schema/scenario_', om_version, '" ',
        'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://openmalaria.org/schema/scenario_', om_version, " scenario_", om_version, '.xsd">',
        "</om:scenario>"
      )
      ## Parse XML string and use it to initialize the scenario_44.xsd
      om_class_name <- paste0("om_", om_version, "_classes")
      ## om_class <- import_from_path(om_class_name)$OM0Scenario
      ## self$scenario <- om_class(etree$XML(xml_str))
      self$xml_tree <- etree$ElementTree(etree$XML(r_to_py(xml_str)$encode("utf-8")))
      self$scenario <- import_from_path(om_class_name)$OM0Scenario(self$xml_tree)
    }
  )
)

my_exp <- OMExperiment$new(44)

OMExperiment$set(
  "public",
  "define_demography", function(maximumAgeYrs, name, popSize, lowerbound, groups) {
    self$scenario$demography$name$set(name)
    self$scenario$demography$maximumAgeYrs$set(maximumAgeYrs)
    self$scenario$demography$popSize$set(popSize)
    self$scenario$demography$ageGroup$lowerbound$set(lowerbound)
    for (row in 1:nrow(groups)) {
      self$scenario$demography$ageGroup$group$add()
      self$scenario$demography$ageGroup$group$poppercent$set(groups[row, "poppercent"])
      self$scenario$demography$ageGroup$group$upperbound$set(groups[row, "upperbound"])
    }
  }
)

demo_groups <- data.frame(
  poppercent = c(
    3.474714994,
    12.76004028,
    14.52151394,
    12.75565434,
    10.836323739,
    8.393312454,
    7.001421452,
    5.800587654,
    5.102136612,
    4.182561874,
    3.339409351,
    2.986112356,
    2.555766582,
    2.332763433,
    1.77400255,
    1.008525491,
    0.74167341,
    0.271863401,
    0.161614642
  ),
  upperbound = c(
    1,
    5,
    10,
    15,
    20,
    25,
    30,
    35,
    40,
    45,
    50,
    55,
    60,
    65,
    70,
    75,
    80,
    85,
    90
  )
)
demo_groups

my_exp$define_demography(name = "Ifakara", maximumAgeYrs = 90, popSize = 2000L, lowerbound = 0, groups = demo_groups)
my_exp$scenario$view()
