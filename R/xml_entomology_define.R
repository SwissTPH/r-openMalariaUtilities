##' Defines entomology setting and writes it baseList file
##' @param baseList List with experiment data.
##' @param MosquitoParameters Mosquito bionomics parameterization list of
##'   different mosquito species as obtained from AnophelesModel package
##'   function get_OM_ento_snippet
##' @param append If TRUE, then append to existing baseList, otherwise overwrite
##' @export
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


## DEPRECATED
##' @title Write entomology
##' @param baseList List with experiment data.
##' @param mosqs Mosquito vectors
##' @param contrib Proportion contribution
##' @param propInfected Proportion infected
##' @param propInfectious Proportion infectious
##' @param EIR EIR value
##' @param seasonality Vector of length 12
##' @export
make_ento_compat <- function(baseList, mosqs,
                             contrib = c(0.81, 0.08, 0.09, 0.02),
                             propInfected = 0.078,
                             propInfectious = 0.021,
                             EIR = "@EIR@",
                             seasonality = paste0("@m", 1:12, "@")) {

  ## Writes entomology section, for funestus, gambiaess, albimaus, arabiensis
  alb <- c(
    0.001, 3, 11, 0.616, 0.33, 0.29, 0, 0.95, 0.95, 0.46083, 0.88, 0.10, 1,
    0.95, 0.95, 0.99
  )
  fun <- c(
    0.001, 3, 11, 0.616, 0.33, 0.623, 0, 0.95, 0.95, 0.99, 0.88, 0.98, 1, 0.95,
    0.95, 0.99
  )
  gam <- c(
    0.001, 3, 11, 0.313, 0.33, 0.623, 0, 0.95, 0.95, 0.99, 0.88, 0.939, 1, 0.95,
    0.95, 0.99
  )
  ara <- c(
    0.001, 3, 11, 0.313, 0.33, 0.623, 0, 0.95, 0.95, 0.99, 0.88, 0.871, 1, 0.95,
    0.95, 0.99
  )

  para <- data.frame(rbind(fun, gam, alb, ara))
  colnames(para) <- c(
    "min", "rest", "inc", "egg", "seek", "surv", "avail", "pbite",
    "psite", "prest", "povi", "hbi", "rel", "nbite", "nsite", "nrest"
  )
  para$mosq <- rownames(para)
  para

  if (length(seasonality) < 12) {
    seasonality <- rep(seasonality[1], 12)
  }

  # Construct monthlyValues
  monthlyValues <- list(
    smoothing = "fourier"
  )
  for (i in seasonality) {
    monthlyValues <- append(monthlyValues, list(value = list(i)))
  }

  # Begin entry
  outlist <- list(
    mode = "dynamic",
    name = "Namawala",
    scaledAnnualEIR = EIR
  )

  ### ---- for each mosquito
  temp <- list()
  for (i in seq_len(length(mosqs))) {
    this <- grep(substr(mosqs[i], start = 1, stop = 3), para$mosq)

    ## Add bionomics information for each mosquito species
    temp <- .xmlAddList(
      data = temp, sublist = NULL,
      entry = "anopheles",
      input = list(
        mosquito = mosqs[i],
        propInfected = propInfected,
        propInfectious = propInfectious,
        seasonality = list(
          input = "EIR",
          annualEIR = contrib[i],
          monthlyValues = monthlyValues
        ),
        mosq = list(
          minInfectedThreshold = para$min[this],
          mosqRestDuration = list(
            value = para$rest[this]
          ),
          extrinsicIncubationPeriod = list(
            value = para$inc[this]
          ),
          mosqLaidEggsSameDayProportion = list(
            value = para$egg[this]
          ),
          mosqSeekingDuration = list(
            value = para$seek[this]
          ),
          mosqSurvivalFeedingCycleProbability = list(
            value = para$surv[this]
          ),
          availability = list(
            distr = "const"
          ),
          mosqProbBiting = list(
            mean = para$pbite[this],
            variance = 0
          ),
          mosqProbFindRestSite = list(
            mean = para$psite[this],
            variance = 0
          ),
          mosqProbResting = list(
            mean = para$prest[this],
            variance = 0
          ),
          mosqProbOvipositing = list(
            value = para$povi[this]
          ),
          mosqHumanBloodIndex = list(
            value = para$hbi[this]
          )
        ),
        nonHumanHosts = list(
          name = "unprotectedAnimals",
          mosqRelativeEntoAvailability = list(
            value = para$rel[this]
          ),
          mosqProbBiting = list(
            value = para$nbite[this]
          ),
          mosqProbFindRestSite = list(
            value = para$nsite[this]
          ),
          mosqProbResting = list(
            value = para$nrest[this]
          )
        )
      )
    )
  }

  ## Tail part of entomology
  temp <- append(temp, list(nonHumanHosts = list(
    name = "unprotectedAnimals",
    number = 1.0
  )))

  outlist <- .xmlAddList(
    data = outlist, sublist = NULL, entry = "vector", input = temp
  )

  baseList <- .xmlAddList(
    data = baseList, sublist = "entomology", entry = NULL, input = outlist
  )

  return(baseList)
}
