##' Defines entomology setting
##' @param baseList List with experiment data.
##' @param seasonalityParameters Seasonality parameterization.
##' @param mosquitoParameters Mosquito bionomics parameterization list of
##'   different mosquito species as obtained from AnophelesModel package
##'   function get_OM_ento_snippet
##' @param mode Dynamic or static
##' @param name Name of your setting
##' @param scaledAnnualEIR If not NULL, annualEIR will be rescaled by numeric
##' @param verbose If TRUE, print detailed output.
##' @param append If TRUE, then append to existing baseList, otherwise overwrite
##' @examples  mosquitoParameters=list("Anopheles gambiae"=list(
##'   mosqRestDuration=list(value="2"),
##'   extrinsicIncubationPeriod=list(value="10"),
##'   mosqLaidEggsSameDayProportion=list(value="0.589"),
##'   mosqSeekingDuration=list(value="3"),
##'   mosqSurvivalFeedingCycleProbability=list(value="0.75"),
##'   mosqProbBiting=list(mean="0.95",
##'                       variance="0"),
##'   mosqProbFindRestSite=list(mean="0.95",
##'                             variance="0"),
##'   mosqProbResting=list(mean="0.99",
##'                        variance="0"),
##'    mosqProbOvipositing=list(mean="0.88"),
##'   mosqHumanBloodIndex=list(mean="0.6243")))
##' @export
defineEntomology <- function(baseList, seasonalityParameters,
                             mosquitoParameters, mode = "dynamic",
                             name = "Namawala", scaledAnnualEIR = NULL,
                             verbose = FALSE, append = TRUE) {

  ## Verify input
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertList(mosquitoParameters)
  checkmate::assertList(seasonalityParameters)
  checkmate::assertSubset(mode,
    choices = c("dynamic", "static"),
    add = assertCol
  )
  checkmate::assertCharacter(name)
  checkmate::assert(
    checkmate::checkCharacter(scaledAnnualEIR, pattern = "@(.*?)@", null.ok = TRUE),
    checkmate::checkNumber(scaledAnnualEIR, null.ok = TRUE)
  )
  checkmate::assertSubset(verbose,
    choices = c(TRUE, FALSE),
    add = assertCol
  )
  checkmate::assertSubset(append,
    choices = c(TRUE, FALSE),
    add = assertCol
  )
  checkmate::reportAssertions(assertCol)

  ## Setup, add scaledAnnualEIR if specified
  setupList <- list(
    mode = mode,
    name = name
  )

  if (!is.null(scaledAnnualEIR)) {
    setupList$scaledAnnualEIR <- scaledAnnualEIR
    if (verbose) {
      message(
        paste0(
          "Adding scaledAnnualEIR ",
          scaledAnnualEIR, " to baseXML file..."
        )
      )
    }
  }

  baseList <- .xmlAddList(
    data = baseList, sublist = "entomology",
    entry = NULL,
    append = append,
    input = setupList
  )

  ## Writing mosquito bionomics and seasonality data per vector species
  for (k in names(mosquitoParameters)) {
    if (verbose) {
      message(
        paste0(
          "Writing mosquito bionomics and seasonality data for vector species ",
          k, " to baseXML file..."
        )
      )
    }
    mosqData <- mosquitoParameters[[k]]
    seasData <- seasonalityParameters[[k]]

    inputList <- list(
      mosquito = k,
      propInfected = seasData$propInfected,
      propInfectious = seasData$propInfectious
    )

    ## Add seasonality part
    if (length(seasData$seasonality) == 1) {
      if (seasData$seasonality == "fourierSeries") {
        inputList <- append(
          inputList,
          list(seasonality = list(
            annualEIR = seasData$annualEIR,
            input = "EIR",
            fourierSeries = list(
              EIRRotateAngle = "0",
              coeffic = list(a = "0.8968", b = "2.678"),
              coeffic = list(a = "-0.4551", b = "2.599")
            )
          ))
        )
      }
    } else if (
      (is.numeric(seasData$seasonality) && length(seasData$seasonality) == 12) ||
        (grepl("^@.*@", seasData$seasonality) && length(seasData$seasonality) == 12)
    ) {
      tmp <- list()
      tmp <- .xmlAddList(
        data = tmp, sublist = NULL,
        entry = NULL,
        input = list(
          smoothing = "fourier"
        )
      )

      for (mo in seasData$seasonality) {
        tmp <- .xmlAddList(
          data = tmp, sublist = NULL,
          entry = "value",
          input = list(mo)
        )
      }

      inputList <- .xmlAddList(
        data = inputList, sublist = "seasonality", entry = NULL,
        input = list(
          annualEIR = seasData$annualEIR,
          input = "EIR",
          monthlyValues = tmp
        )
      )
    } else if (
      (is.numeric(seasData$seasonality) && length(seasData$seasonality) == 365) ||
        (grepl("^@.*@", seasData$seasonality) && length(seasData$seasonality) == 365)
    ) {
      tmp <- list()
      tmp <- .xmlAddList(
        data = tmp, sublist = NULL,
        entry = NULL,
        input = list(
          smoothing = "fourier"
        )
      )

      for (da in seasData$seasonality) {
        tmp <- .xmlAddList(
          data = tmp, sublist = NULL,
          entry = "value",
          input = list(da)
        )
      }

      inputList <- .xmlAddList(
        data = inputList, sublist = "seasonality", entry = NULL,
        input = list(
          annualEIR = seasData$annualEIR,
          input = "EIR",
          dailyValues = tmp
        )
      )
    } else {
      stop("Seasonality needs to be either 'fourierSeries', a numeric vector or
           a vector of placeholders '@foo@' of length 12 or 365")
    }

    ## Add mosq part
    inputList <- .xmlAddList(
      data = inputList, sublist = NULL, entry = "mosq",
      input = list(
        minInfectedThreshold = 0.001,
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
        availability = list(distr = "const"),
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
      )
    )

    ## Add nonHumanHosts part
    inputList <- .xmlAddList(
      data = inputList, sublist = NULL, entry = "nonHumanHosts",
      input = list(
        name = "unprotectedAnimals",
        mosqRelativeEntoAvailability = list(value = 1.0),
        mosqProbBiting = list(value = 0.95),
        mosqProbFindRestSite = list(value = 0.95),
        mosqProbResting = list(value = 0.99)
      )
    )

    ## Add bionomics information for each mosquito species
    baseList <- .xmlAddList(
      data = baseList, sublist = c("entomology", "vector"),
      entry = "anopheles",
      append = append,
      input = inputList
    )
  }

  ## Add non-human hosts
  baseList <- .xmlAddList(
    data = baseList, sublist = c("entomology", "vector"),
    entry = "nonHumanHosts",
    input = list(name = "unprotectedAnimals", number = 1.0)
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

  ## Add bionomics information for each mosquito species
  temp <- list()
  for (i in seq_len(length(mosqs))) {
    this <- grep(substr(mosqs[i], start = 1, stop = 3), para$mosq)

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
