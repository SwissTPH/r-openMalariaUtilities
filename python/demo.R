library(reticulate)

setwd("~/Projekte/r-openMalariaUtilities/python/")

## source_python("demo_class.py")

demo <- import_from_path("demo_class")

my_cool_experiment <- demo$OMExperiment(44L)

my_cool_experiment$scenario$demography$help()

my_cool_experiment$scenario$help()

my_cool_experiment$scenario$demography$name$set("blaaa")

my_cool_experiment$scenario$demography$popSize$set(10000)

my_cool_experiment$scenario$demography$ageGroup$lowerbound$set(0)

my_cool_experiment$scenario$demography$ageGroup$group$add()

my_cool_experiment$scenario$demography$ageGroup$group$poppercent$set(3.4)
my_cool_experiment$scenario$demography$ageGroup$group$upperbound$set(1)

my_cool_experiment$scenario$demography$ageGroup$group$add()
my_cool_experiment$scenario$demography$ageGroup$group$poppercent$set(3.5)
my_cool_experiment$scenario$demography$ageGroup$group$upperbound$set(1)

my_cool_experiment$scenario$demography$ageGroup$group$select(upperbound = "1.0", poppercent = "3.5")
my_cool_experiment$scenario$demography$ageGroup$group$upperbound$set(5)

my_cool_experiment$scenario$demography$ageGroup$group$upperbound$update(10, upperbound = "5.0", poppercent = "3.5")

my_cool_experiment$scenario$demography$ageGroup$clear()

xml_str <- '<demography maximumAgeYrs="90" name="Ifakara" popSize="2000"> \
        <ageGroup lowerbound="0"> \
            <group poppercent="3.474714994" upperbound="1"/> \
            <group poppercent="12.76004028" upperbound="5"/> \
            <group poppercent="14.52151394" upperbound="10"/> \
            <group poppercent="12.75565434" upperbound="15"/> \
            <group poppercent="10.836323739" upperbound="20"/> \
            <group poppercent="8.393312454" upperbound="25"/> \
            <group poppercent="7.001421452" upperbound="30"/> \
            <group poppercent="5.800587654" upperbound="35"/> \
            <group poppercent="5.102136612" upperbound="40"/> \
            <group poppercent="4.182561874" upperbound="45"/> \
            <group poppercent="3.339409351" upperbound="50"/> \
            <group poppercent="2.986112356" upperbound="55"/> \
            <group poppercent="2.555766582" upperbound="60"/> \
            <group poppercent="2.332763433" upperbound="65"/> \
            <group poppercent="1.77400255" upperbound="70"/> \
            <group poppercent="1.008525491" upperbound="75"/> \
            <group poppercent="0.74167341" upperbound="80"/> \
            <group poppercent="0.271863401" upperbound="85"/> \
            <group poppercent="0.161614642" upperbound="90"/> \
        </ageGroup> \
    </demography>'

my_cool_experiment$scenario$demography$from_xml(xml_str)

my_cool_experiment$scenario$view()

define_demography <- function(x, maximumAgeYrs, name, popSize, lowerbound, groups) {
  x$scenario$demography$name$set(name)
  x$scenario$demography$maximumAgeYrs$set(maximumAgeYrs)
  x$scenario$demography$popSize$set(popSize)
  x$scenario$demography$ageGroup$lowerbound$set(lowerbound)
  for (row in seq_len(nrow(groups))) {
    x$scenario$demography$ageGroup$group$add()
    x$scenario$demography$ageGroup$group$poppercent$set(groups[row, "poppercent"])
    x$scenario$demography$ageGroup$group$upperbound$set(groups[row, "upperbound"])
  }
}

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

define_demography(my_cool_experiment, name = "Ifakara", maximumAgeYrs = 90, popSize = 2000L, lowerbound = 0, groups = demo_groups)
my_cool_experiment$scenario$view()

my_cool_experiment$write_xml("output.xml")
