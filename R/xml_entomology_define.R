##' Defines entomology setting
##' @param baseList List with experiment data.
##' @param MosquitoParameters Mosquito bionomics parameterization list of
##'   different mosquito species as obtained from AnophelesModel package
##'   function get_OM_ento_snippet
##' @param append If TRUE, then append to existing baseList, otherwise overwrite
defineEntomology <- function(baseList, MosquitoParameters, append = TRUE) {
  ## REVIEW Is this supposed to be an example considered by Roxygen?
  ## Example:
  ## MosquitoParameters=list("Anopheles gambiae"=list(
  ##   mosqRestDuration=list(value="2"),
  ##   extrinsicIncubationPeriod=list(value="10"),
  ##   mosqLaidEggsSameDayProportion=list(value="0.589"),
  ##   mosqSeekingDuration=list(value="3"),
  ##   mosqSurvivalFeedingCycleProbability=list(value="0.75"),
  ##   mosqProbBiting=list(mean="0.95",
  ##                       variance="0"),
  ##   mosqProbFindRestSite=list(mean="0.95",
  ##                             variance="0"),
  ##   mosqProbResting=list(mean="0.99",
  ##                        variance="0"),
  ##    mosqProbOvipositing=list(mean="0.88"),
  ##   mosqHumanBloodIndex=list(mean="0.6243")))

  for (k in names(MosquitoParameters)) {
    mosqData <- MosquitoParameters[[k]]

    ## Add bionomics information for each mosquito species
    baseList <- .xmlAddList(
      data = baseList, sublist = c("entomology", "vector"),
      entry = "anopheles",
      append = append,
      input = list(
        mosquito = k,
        propInfected = 0.078,
        propInfectious = "0.021",
        seasonality = list(
          annualEIR = "15",
          input = "EIR",
          fourierSeries = list(
            EIRRotateAngle = "0",
            coeffic = list(a = "0.8968", "b" = "2.678"),
            coeffic = list(a = "-0.4551", b = "2.599")
          )
        ),
        mosq = list(
          minInfectedThreshold = "0.001",
          mosqRestDuration = list(
            value = mosqData[["mosqRestDuration"]][["value"]]
          ),
          extrinsicIncubationPeriod = list(
            value = mosqData[["extrinsicIncubationPeriod"]][["value"]]
          ),
          mosqLaidEggsSameDayProportion = list(
            value = mosqData[["mosqLaidEggsSameDayProportion"]][["value"]]
          ),
          mosqSeekingDuration = list(
            value = mosqData[["mosqSeekingDuration"]][["value"]]
          ),
          mosqSurvivalFeedingCycleProbability = list(
            value = mosqData[["mosqSurvivalFeedingCycleProbability"]][["value"]]
          ),
          availability = list(distr = "const"), ## need to ask Monica
          mosqProbBiting = list(
            mean = mosqData[["mosqProbBiting"]][["mean"]],
            variance = mosqData[["mosqProbBiting"]][["variance"]]
          ),
          mosqProbFindRestSite = list(
            mean = mosqData[["mosqProbFindRestSite"]][["mean"]],
            variance = mosqData[["mosqProbFindRestSite"]][["variance"]]
          ),
          mosqProbResting = list(
            mean = mosqData[["mosqProbResting"]][["mean"]],
            variance = mosqData[["mosqProbResting"]][["variance"]]
          ),
          mosqProbOvipositing = list(
            value = mosqData[["mosqProbOvipositing"]][["mean"]]
          ),
          mosqHumanBloodIndex = list(
            value = mosqData[["mosqHumanBloodIndex"]][["mean"]]
          )
        ),
        nonHumanHosts = list(
          name = "unprotectedAnimals",
          mosqRelativeEntoAvailability = list(value = "1.0"),
          mosqProbBiting = list(value = "0.95"),
          mosqProbFindRestSite = list(value = "0.95"),
          mosqProbResting = list(value = "0.99")
        )
      )
    )
  }

  ## Add non-human hosts
  baseList <- .xmlAddList(
    data = baseList, sublist = c("entomology", "vector"),
    entry = "nonHumanHosts",
    input = list(name = "unprotectedAnimals", number = "1.0")
  )

  return(baseList)
}

##' @rdname defineEntomology
##' @export
define_entomology <- defineEntomology
