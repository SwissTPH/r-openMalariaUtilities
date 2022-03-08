##  TODO Improve documentation
##' Adds vaccine intervention parameterisation to baseList
##' @param baseList List with experiment data.
##' @param vaccineParameterization Parameterization list
##' @param append If TRUE, then append to existing baseList, otherwise
##'   overwrites
##' @param name Name tag list
##' @param verbatim If TRUE, then show messages
##' @param hist If TRUE, then decay is assumed to be step function set to 1 for
##'   a year and then to zero for the remainder
##' @export
defineVaccine <- function(baseList, vaccineParameterization, append = TRUE,
                          name = NULL, verbatim = FALSE, hist = FALSE) {

  ## Verify input
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertSubset(verbatim,
    choices = c(TRUE, FALSE),
    add = assertCol
  )
  checkmate::assertSubset(hist,
    choices = c(TRUE, FALSE),
    add = assertCol
  )
  checkmate::reportAssertions(assertCol)

  ## Check name argument
  if (!is.null(name) && names(name) != names(vaccineParameterization)) {
    stop(
      "Names of vaccineParameterization and name arguments need to be the same."
    )
  }

  ## Make sure interventions header is set
  baseList <- .defineInterventionsHeader(baseList = baseList)

  ## Create intervention cohort
  for (k in names(vaccineParameterization)) {
    baseList <- .xmlAddList(
      data = baseList,
      sublist = c("interventions", "human"),
      append = append,
      entry = "component",
      input = list(
        id = paste0(k, "-intervention_cohort"),
        recruitmentOnly = list(),
        subPopRemoval = list(afterYears = "5")
      )
    )
    if (verbatim) {
      message(
        paste0(
          "Writing intervention cohort component ",
          paste0(k, "_intervention_cohort"), " to baseXML file..."
        )
      )
    }

    ## Add vaccine mode of action (PEV,BSV,TBV are mutually exclusive?) and
    ## define component id
    mode_of_action_list <- list(
      id = k, name = if (is.null(name)) "your_tag" else name[[k]]
    )
    for (mode_of_action in names(vaccineParameterization[[k]])) {
      mode_of_action_list <- append(
        mode_of_action_list, vaccineParameterization[[k]][mode_of_action]
      )
      if (verbatim) {
        message(
          paste0(
            "Writing vaccine component ", k, " with mode of action ",
            mode_of_action, " to baseXML file..."
          )
        )
      }
    }

    baseList <- .xmlAddList(
      data = baseList,
      sublist = c("interventions", "human"),
      append = append,
      entry = "component",
      input = mode_of_action_list
    )
  }

  return(baseList)
}

##' @rdname defineVaccine
##' @export
define_vaccine <- defineVaccine


##' Adds vector control intervention parameterisation to baseList
##' @param baseList List with experiment data.
##' @param vectorInterventionParameters Vector control intervention
##'   parameterization list depending on three parameters (deterrency,
##'   preprandrial, postprandial) and decay functions:
##' @param append If TRUE, then append to existing baseList, otherwise
##'   overwrites
##' @param name Name tag list
##' @param verbatim If TRUE, then show messages
##' @param hist If TRUE, then decay is assumed to be step function set to 1 for
##'   a year and then to zero for the remainder
##' @param resistance Scaling function of insecticide resistance TODO
##' @examples
##' vectorInterventionParameters <- list(
##'   "LLIN" = list(
##'     deterrency = list(
##'       decay = list(
##'         L = "0.7",
##'         "function" = "weibull"
##'       ),
##'       anophelesParams = list(
##'         "Anopheles gambiae" = list(
##'           propActive = 1,
##'           value = "0.5"
##'         ),
##'         "Anopheles funestus" = list(
##'           propActive = 1,
##'           value = "0.3"
##'         )
##'       )
##'     ),
##'     preprandialKillingEffect = list(
##'       decay = list(
##'         L = "0.4",
##'         "function" = "weibull"
##'       ),
##'       anophelesParams = list(
##'         "Anopheles gambiae" = list(
##'           propActive = 1,
##'           value = "0.6"
##'         ),
##'         "Anopheles funestus" = list(
##'           propActive = 1,
##'           value = "0.67"
##'         )
##'       )
##'     ),
##'     postprandialKillingEffect = list(
##'       decay = list(
##'         L = "0.45",
##'         "function" = "weibull"
##'       ),
##'       anophelesParams = list(
##'         "Anopheles gambiae" = list(
##'           propActive = 1,
##'           value = "0.3"
##'         ),
##'         "Anopheles funestus" = list(
##'           propActive = 1,
##'           value = "0.2"
##'         )
##'       )
##'     )
##'   )
##' )
##' @export
defineVectorControl <- function(baseList, vectorInterventionParameters,
                                append = TRUE, name = NULL, verbatim = TRUE,
                                hist = FALSE, resistance = 0.1) {

  ## Verify input
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertSubset(hist,
    choices = c(TRUE, FALSE),
    add = assertCol
  )
  checkmate::assertNumeric(
    resistance,
    lower = 0, upper = 1, null.ok = TRUE, add = assertCol
  )
  checkmate::reportAssertions(assertCol)

  ## Some checks
  if (is.null(baseList$interventions$human)) {
    stop("To append, the baseList needs a child called '$interventions$human'")
  }

  ## Check whether vector species in entomology section of baseList and those in
  ## vector control interventions are the same
  ## REVIEW These conditionals look overly complex
  for (intervention in names(vectorInterventionParameters)) {
    for (effect in names(vectorInterventionParameters[[intervention]])) {
      if (!setequal(
        names(vectorInterventionParameters[[intervention]][[effect]][["anophelesParams"]]),
        unique(unlist(lapply(baseList$entomology$vector, function(x) x$mosquito)))
      )) {
        stop("To append, each vector species definied in the entomology section must be the same as in the intervention component.")
      }
    }
  }

  ## Make sure interventions header is set
  baseList <- .defineInterventionsHeader(baseList = baseList)

  ## Loop over interventions, effects and vector species
  for (k in names(vectorInterventionParameters)) {
    componentData <- vectorInterventionParameters[[k]]

    for (effect in names(componentData)) {
      component_id <- paste0(k, ifelse(hist, "hist", ""), "-", effect)
      if (verbatim) {
        message(
          paste0("Defining intervention with component_id: ", component_id)
        )
      }

      GVIList <- list(decay = if (hist) {
        list("L" = 1, "function" = "step")
      } else {
        componentData[[effect]][["decay"]]
      })

      for (vector_species in names(componentData[[effect]]$anophelesParams)) {
        print(
          paste0("Writing effect values for vector species: ", vector_species)
        )
        values <- c(
          deterrency = 0,
          preprandialKillingEffect = 0,
          postprandialKillingEffect = 0
        )
        values[effect] <- componentData[[effect]][["anophelesParams"]][[vector_species]][["value"]]

        GVIList <- append(
          GVIList,
          list(anophelesParams = list(
            mosquito = vector_species,
            propActive = componentData[[effect]][["anophelesParams"]][[vector_species]][["propActive"]],
            deterrency = list(value = values[["deterrency"]]),
            preprandialKillingEffect = list(value = values[["preprandialKillingEffect"]]),
            postprandialKillingEffect = list(value = values[["postprandialKillingEffect"]])
          ))
        )
      }

      ## Add to list
      baseList <- .xmlAddList(
        data = baseList, sublist = c("interventions", "human"), append = append,
        entry = "component",
        input = list(
          id = component_id,
          name = if (is.null(name)) "your_tag" else name[[k]],
          GVI = GVIList
        )
      )
    }
  }

  return(baseList)
}


##' @rdname defineVectorControl
##' @export
define_vector_control <- defineVectorControl


## https://swisstph.github.io/openmalaria/schema-43.html#elt-component-2
## https://swisstph.github.io/openmalaria/schema-43.html#elt-GVI
##' @title Adds the IRS intervention parameterisation
##' @param baseList List with experiment data.
##' @param mosquitos Mosquito species affected by the intervention.
##' @param component Insecticde to use. This will automatically specify efficacy
##'   and effect duration. Needs to from predefined choices.
##' @param noeffect Pattern which defines which mosquitoes are unaffected by
##'   intervention. Can be "outdoor", "indoor" or NULL.
##' @param steplife Step function decay, the number of months it's effective.
##' @export
defineIRS <- function(baseList, mosquitos, component = c(
                        "Actellic50EC", "Actellic300CS",
                        "Actellic11", "Actellic8",
                        "Actellic80", "magic_actellic",
                        "IRS4", "IRS6", "IRS8",
                        "Bendiocarb", "nothing"
                      ),
                      noeffect = "outdoor", steplife = NULL) {
  # Verify input
  component <- match.arg(component)
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertSubset(noeffect,
    choices = c("indoor", "outdoor"),
    add = assertCol
  )
  checkmate::assertNumeric(steplife, lower = 0, null.ok = TRUE, add = assertCol)
  checkmate::reportAssertions(assertCol)

  ## Component decay data
  ## List which contains:
  ## - the component name
  ## - 'vals' is a vector with the deterrency, preprandialKillingEffect and
  ##   postprandialKillingEffect value, in this order
  ## - decay L value
  ## - function type
  componentData <- list(
    "Actellic50EC" = list(
      vals = c(-0.28, 0.23, 0.38),
      decayL = 0.299,
      k = 1.51,
      "function" = "weibull"
    ),
    "Actellic300CS" = list(
      vals = c(-0.28, 0.23, 0.38),
      decayL = 0.448,
      k = 1.11,
      "function" = "weibull"
    ),
    "Actellic11" = list(
      vals = c(-0.28, 0.23, 0.38),
      decayL = 0.917,
      "function" = "step"
    ),
    "Actellic8" = list(
      vals = c(-0.28, 0.23, 0.38),
      decayL = 0.67,
      "function" = "step"
    ),
    "Actellic80" = list(
      vals = c(-0.28, 0.23, 0.38),
      decayL = 0.67,
      k = 20,
      "function" = "weibull"
    ),
    "magic_actellic" = list(
      vals = c(-0.28, 0.23, 0.38),
      decayL = 0.448,
      k = 1.11,
      "function" = "weibull"
    ),
    "IRS4" = list(
      vals = c(-0.28, 0.23, 0.38),
      decayL = 0.333,
      "function" = "step"
    ),
    "IRS6" = list(
      vals = c(-0.28, 0.23, 0.38),
      decayL = 0.5,
      "function" = "step"
    ),
    "IRS8" = list(
      vals = c(-0.28, 0.23, 0.38),
      decayL = 0.667,
      "function" = "step"
    ),
    "Bendiocarb" = list(
      vals = c(0.15, 0, 0.80),
      decayL = 0.25,
      "function" = "exponential"
    ),
    "nothing" = list(
      vals = c(0, 0, 0),
      decayL = 1,
      "function" = "step"
    )
  )

  ## If steplife is given, use step function
  if (!is.null(steplife)) {
    componentData[[component]][["decayL"]] <- round(steplife / 12, 3)
    componentData[[component]][["function"]] <- "step"
    message(paste(
      "Decay function will be a step function with a duration of", steplife,
      "months"
    ))
  }

  ## Add decay information
  outlist <- list()
  outlist <- .xmlAddList(
    data = outlist, sublist = NULL,
    entry = NULL,
    input = list(
      id = component,
      name = component,
      GVI = list(
        decay = list(
          L = componentData[[component]][["decayL"]],
          "function" = componentData[[component]][["function"]]
        )
      )
    )
  )

  ## propActive: Proportion of bites for which IRS acts, defaults to 1
  propActive <- rep(1, length(mosquitos))
  ## Set IRS effectiveness to 0 for mosquitos which have 'noeffect' in their
  ## name, if given
  if (!is.null(noeffect)) {
    propActive[grep(mosquitos, pattern = noeffect)] <- 0
  }
  ## If 'component' is 'nothing', set propActive to zero
  if (component == "nothing") {
    propActive <- rep(0, length(mosquitos))
  }

  ## Add mosquito information
  for (i in seq_len(length(mosquitos))) {
    outlist <- .xmlAddList(
      data = outlist, sublist = c("GVI"),
      entry = "anophelesParams",
      input = list(
        mosquito = mosquitos[[i]],
        propActive = propActive[[i]],
        deterrency = list(value = componentData[[component]][["vals"]][[1]]),
        preprandialKillingEffect = list(
          value = componentData[[component]][["vals"]][[2]]
        ),
        postprandialKillingEffect = list(
          value = componentData[[component]][["vals"]][[3]]
        )
      )
    )
  }

  ## Make sure interventions header is set
  baseList <- .defineInterventionsHeader(baseList = baseList)

  baseList <- .xmlAddList(
    data = baseList, sublist = c("interventions", "human"),
    entry = "component", input = outlist
  )

  return(baseList)
}

##' @rdname defineIRS
##' @export
define_IRS <- defineIRS

## DEPRECATED
##' @title Adds the IRS intervention parameterisation. Compatibility version.
##' @param baseList List with experiment data.
##' @param mosqs Mosquito species affected by the intervention.
##' @param component Insecticde to use. This will automatically specify efficacy
##'   and effect duration. Needs to from predefined choices.
##' @param noeffect Pattern which defines which mosquitoes are unaffected by
##'   intervention. Can be "outdoor", "indoor" or NULL.
##' @param steplife Step function decay, the number of months it's effective.
##' @export
define_IRS_compat <- function(baseList, mosqs, component = "Actellic50EC",
                              noeffect = "outdoor", steplife = NULL) {
  baseList <- defineIRS(
    baseList = baseList, mosquitos = mosqs, component = component,
    noeffect = noeffect, steplife = steplife
  )

  return(baseList)
}


## https://swisstph.github.io/openmalaria/schema-43.html#elt-treatSimple-3

##' @title SMC, MDA parameterization section
##' @description Writes the treatSimple intervention used for mass treatments
##'   (i.e. MDA, SMC)
##' @param baseList List with experiment data.
##' @param component Name of the intervention, can be any name but needs to be
##'   the same as defined in deployment
##' @param durationBlood Clearance of blood stage parasites
##' @param durationLiver Clearance of liver stage parasites
##' @export
defineTreatSimple <- function(baseList, component = "MDA",
                              durationBlood = "15d", durationLiver = 0) {
  # Verify input
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(component, add = assertCol)
  checkmate::reportAssertions(assertCol)

  ## Make sure interventions header is set
  baseList <- .defineInterventionsHeader(baseList = baseList)

  ## Add information
  baseList <- .xmlAddList(
    data = baseList, sublist = c("interventions", "human"),
    entry = "component",
    input = list(
      id = component,
      name = component,
      treatSimple = list(
        durationBlood = durationBlood,
        durationLiver = durationLiver
      )
    )
  )

  return(baseList)
}

##' @rdname defineTreatSimple
##' @export
define_treatSimple <- defineTreatSimple

## DEPRECATED
##' @rdname defineTreatSimple
##' @title SMC, MDA parameterization section. Compatibility version.
##' @export
define_treatSimple_compat <- defineTreatSimple

##' @title Writes an intervention parameterisation xml chunk that does nothing
##' @description This is useful if something needs to be deployed, as a
##'   placeholder.
##' @param baseList List with experiment data.
##' @param mosquitos Name of mosquito species affected by the intervention.
##' @export
defineNothing <- function(baseList, mosquitos) {

  ## This is simply a subset of defineIRS
  baseList <- defineIRS(
    baseList = baseList, mosquitos = mosquitos,
    component = "nothing", noeffect = NULL
  )

  return(baseList)
}

##' @rdname defineNothing
##' @export
define_nothing <- defineNothing

## DEPRECATED
##' @title Writes an intervention parameterisation xml chunk that does nothing.
##'   Compatibility version.
##' @description This is useful if something needs to be deployed, as a
##'   placeholder.
##' @param baseList List with experiment data.
##' @param mosqs Name of mosquito species affected by the intervention.
##' @param component Name of the intervention, can be any name but needs to be
##'   the same as defined in deployment
##' @export
define_nothing_compat <- function(baseList, mosqs, component = "nothing") {

  ## This is simply a subset of defineIRS
  baseList <- defineIRS(
    baseList = baseList, mosquitos = mosqs, component = "nothing",
    noeffect = NULL
  )

  return(baseList)
}


##' @title Calculate smooth compact decay parameters
##' @param k Shape parameter
##' @param L Decay parameter (number of years)
##' @param t Vector of time (years)
.calculateSmoothCompact <- function(k, L, t = seq(0, 10, length.out = 100)) {
  # Verify input
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertNumeric(k, lower = 0, add = assertCol)
  checkmate::assertNumeric(L, lower = 0, upper = 10, add = assertCol)
  checkmate::reportAssertions(assertCol)
  if (k <= 0) {
    stop("k needs to be > 0.")
  }

  d <- ifelse(t < L, exp(k - k / (1 - (t / L)^2)), 0)
  return(d)
}

##' @title Calculate smooth compact halflife
##' @description Explores a space of k x L to find the smooth-compact closest to
##'   the given smooth-compact, but with a halflife of 'halflife' (say 2 instead
##'   of 3)
##' @param k Scaling factor in smooth compact
##' @param L Other factor in smooth compact
##' @param halflife Desired halflife (positive number)
##' @param threshold Set to 0.50 for 'halflife'
##' @param grid_n How fine of a grid to explore (50 x 50)
.calculateSmoothCompactHalflife <- function(k = 2.14, L = 6.08, halflife = 2,
                                            threshold = 0.50, grid_n = 50) {
  t0 <- seq(0, 10, length.out = 100)
  d <- .calculateSmoothCompact(k = k, L = L, t = t0)
  half <- t0[min(which(d < threshold))]

  ## The idea is to keep the original shape of the curve but where time is
  ## scaled (sped up, or slowed down) to have a different half-life time point
  ## Constant ratio
  fact <- half / halflife
  t1 <- t0 * fact
  newd <- .calculateSmoothCompact(k = k, L = L, t = t1)

  ## Searching a grid for the parameters with the smallest MSE
  params <- expand.grid(
    k = seq(1, 8, length.out = grid_n),
    L = seq(1, 8, length.out = grid_n)
  )
  ## Comparing the 'original scale' (try) with the target values (newd)
  for (j in seq_len(nrow(params))) {
    try <- .calculateSmoothCompact(k = params$k[j], L = params$L[j], t = t0)
    params[j, 3] <- mean(((try - newd)^2))
  }
  colnames(params)[3] <- "mse"

  ## Finding the minimum parameters
  these <- params[which.min(params$mse), ]

  return(these)
}

## https://swisstph.github.io/openmalaria/schema-43.html#elt-ITN

##' @title Writes the ITN intervention parameterisation xml chunk
##' @param baseList List with experiment data.
##' @param component Name of the intervention, can be any name but needs to be
##'   the same as defined in deployment.
##' @param mosquitos Name of mosquito species affected by the intervention
##' @param halflife Attrition of nets in years
##' @param resist If TRUE, a pyrethroid resistance will be assumed (default
##'   percentage..?)
##' @param historical Used for historical intervention coverage?
##' @param noeffect Which mosquitoes unaffected by intervention?
##' @param strong If !strong and !resist, then "Pitoa" parameter for LLIN
##' @export
defineITN <- function(baseList, component = "histITN", noeffect = "outdoor",
                      mosquitos, halflife = 2, resist = TRUE,
                      historical = FALSE, strong = FALSE) {
  ## Parameters for
  ## https://swisstph.github.io/openmalaria/schema-43.html#elt-ITN, in that
  ## order
  parameterNames <- c(
    "usage", "holeRateMean", "holeRateCV", "ripRateMean",
    "ripRateCV", "ripFactor", "initialsecticideMean",
    "initialsecticideSD", "insecticideDecayL",
    "insecticideDecayMu", "insecticideDecayCV"
  )
  if (!historical) {
    values <- data.frame(
      parameterNames = parameterNames,
      values = c(
        1, 1.8, signif(sqrt(exp(0.8^2) - 1), 4),
        1.8, signif(sqrt(exp(0.8^2) - 1), 4), 0.3,
        55.5, 14, 3, -0.32,
        signif(sqrt(exp(0.8^2) - 1), 4)
      )
    )
    insecticideDecayFun <- "exponential"
  } else {
    values <- data.frame(
      parameterNames = parameterNames,
      values = c(1, 0, 0, 0, 0, 0, 55.5, 14, 1, 0, 0)
    )
    insecticideDecayFun <- "step"
  }

  ## REVIEW Where is this coming from?
  ##        Insecticide decay
  if (component == "monthITN") {
    values[values[, "parameterNames"] == "insecticideDecayK", "values"] <- 0.083
  }

  ## Net attrition
  if (historical == FALSE) {
    hh <- .calculateSmoothCompactHalflife(halflife = halflife)
  }

  ## Add data to baseList
  outlist <- list()
  outlist <- .xmlAddList(
    data = outlist, sublist = NULL,
    entry = NULL,
    input = list(
      id = component,
      name = component,
      ITN = list(
        usage = list(
          value = values[values[, "parameterNames"] == "usage", "values"]
        ),
        holeRate = list(
          CV = values[values[, "parameterNames"] == "holeRateCV", "values"],
          distr = "lognormal",
          mean = values[values[, "parameterNames"] == "holeRateMean", "values"]
        ),
        ripRate = list(
          CV = values[values[, "parameterNames"] == "ripRateCV", "values"],
          distr = "lognormal",
          mean = values[values[, "parameterNames"] == "ripRateMean", "values"]
        ),
        ripFactor = list(
          value = values[values[, "parameterNames"] == "ripFactor", "values"]
        ),
        initialInsecticide = list(
          mean = values[values[, "parameterNames"] == "initialsecticideMean", "values"],
          SD = values[values[, "parameterNames"] == "initialsecticideSD", "values"],
          distr = "normal"
        ),
        insecticideDecay = list(
          "function" = insecticideDecayFun,
          L = values[values[, "parameterNames"] == "insecticideDecayL", "values"],
          CV = values[values[, "parameterNames"] == "insecticideDecayCV", "values"]
        ),
        ## REVIEW 'monthITN' takes precedence (why?), then 'historial' if TRUE,
        ##        else 'historical' == FALSE (default)
        attritionOfNets = if (component == "monthITN") {
          list(
            "function" = "step",
            L = 0.083
          )
        } else if (historical == TRUE) {
          list(
            "function" = "step",
            L = 1
          )
        } else {
          list(
            "function" = "smooth-compact",
            L = hh$L,
            k = hh$k
          )
        }
      )
    )
  )

  ## Mosquito parameters (resistance or not).
  ## Parameters from Briet 2013 Malaria Journal, Supplementary Table 2
  if (resist == TRUE) {
    ## Kou (P2)
    val <- c(
      0.001, 0.003, 0.876, -0.406, 0.018, -0.107, 0.2, 0.268, 0.036, 0.053,
      0.016, 0.413, 0.097, 0.208, 0.014, 0, 0, 0.265, 0.032, 0
    )
  }

  if (resist == FALSE & strong == TRUE) {
    ## PBOnet (Zeneti P2)
    val <- c(
      0.768, 0.2, 0.543, -0.413, 0.012, 0.383, 0.052, 0.322, 0.06, 0.084, 0.016,
      0.899, 0.096, -0.058, 0.28, 0, 0, 0.389, 0.2, 0
    )
  }


  if (resist == FALSE & strong == FALSE) {
    ## PBOweak (Pitoa P2)
    val <- c(
      0.018, 0.005, 0.735, -0.477, 0.014, 0.264, 0.017, 0.476, 0.121, 0.145,
      0.015, 0.682, 0.133, -0.026, 0.067, 0, 0, 0.496, 0.104, 0
    )
  }

  ## propActive: Proportion of bites for which ITN acts, defaults to 1
  propActive <- rep(1, length(mosquitos))
  ## Set IRS effectiveness to 0 for mosquitos which have 'noeffect' in their
  ## name, if given
  for (k in seq_len(length(noeffect))) {
    propActive[grep(mosquitos, pattern = noeffect[k])] <- 0
  }

  ## Loop over mosquitoes and add information to list
  for (i in seq_len(length(mosquitos))) {
    outlist <- .xmlAddList(
      data = outlist, sublist = c("ITN"),
      entry = "anophelesParams",
      input = list(
        mosquito = mosquitos[i],
        propActive = propActive[i],
        twoStageDeterrency = list(
          entering = list(
            insecticideFactor = val[1],
            insecticideScalingFactor = val[2]
          ),
          attacking = list(
            insecticideFactor = val[6],
            insecticideScalingFactor = val[7],
            holeFactor = val[4],
            interactionFactor = val[8],
            holeScalingFactor = val[5],
            baseFactor = val[3]
          )
        ),
        preprandialKillingEffect = list(
          insecticideFactor = val[12],
          insecticideScalingFactor = val[13],
          holeFactor = val[10],
          interactionFactor = val[14],
          holeScalingFactor = val[11],
          baseFactor = val[9]
        ),
        postprandialKillingEffect = list(
          insecticideFactor = val[18],
          insecticideScalingFactor = val[19],
          holeFactor = val[16],
          interactionFactor = val[20],
          holeScalingFactor = val[17],
          baseFactor = val[15]
        )
      )
    )
  }

  ## Make sure interventions header is set
  baseList <- .defineInterventionsHeader(baseList = baseList)

  baseList <- .xmlAddList(
    data = baseList, sublist = c("interventions", "human"),
    entry = "component", input = outlist
  )

  return(baseList)
}

##' @rdname defineITN
##' @export
define_ITN <- defineITN

## DEPRECATED
##' @title Writes the ITN intervention parameterisation xml chunk. Compatibility
##'   version.
##' @param baseList List with experiment data.
##' @param component Name of the intervention, can be any name but needs to be
##'   the same as defined in deployment.
##' @param mosqs Name of mosquito species affected by the intervention
##' @param halflife Attrition of nets in years
##' @param resist If TRUE, a pyrethroid resistance will be assumed (default
##'   percentage..?)
##' @param hist Used for historical intervention coverage?
##' @param noeffect Which mosquitoes unaffected by intervention?
##' @param strong If !strong and !resist, then "Pitoa" parameter for LLIN
##' @param versionnum OpenMalaria version (e.g. 38, 43)
##' @export
define_ITN_compat <- function(baseList, component = "histITN",
                              noeffect = "outdoor", mosqs, halflife = 2,
                              resist = TRUE, hist = FALSE, strong = FALSE,
                              versionnum = 38) {
  baseList <- defineITN(
    baseList = baseList, component = component,
    noeffect = noeffect, mosquitos = mosqs,
    halflife = halflife, resist = resist, historical = hist,
    strong = strong
  )

  return(baseList)
}


##' @title Function that writes the larviciding component
##' @param baseList List with experiment data.
##' @param mosquitos Names of mosquito species to simulate.
##' @param component Name of vector population intervention (i.e. LSM).
##' @param coverage A variable or a numeric value. Can be a placeholder.
##' @param decayVals Decay (if not specified, constant effectiveness, step
##'   function as default)
##' @param startDate Date in YYYY-MM-DD format.
##' @param endDate Date in YYYY-MM-DD format.
##' @param interval A string like '1 weeks'. Same as in [seq.Date()]. Or a list
##'   composed of the entries 'days' (optional), 'months' (optional) and
##'   'years'. If a list is used, startDate and endDate are not used and can be
##'   NULL.
##' @param dates If NULL, startDate, endDate and interval are used, else a
##'   vector of dates in YYYY-MM-DD format. Can be a placeholder.
##' @export
defineLarv <- function(baseList, mosquitos, component = "LSM",
                       coverage = "@futLSMcov@",
                       decayVals = list(L = 0.25, k = NULL, funct = "step"),
                       startDate = NULL, endDate = NULL, interval,
                       dates = NULL) {

  ## Set decay values accordingly
  if (is.null(decayVals)) {
    decay <- list(L = 0.25, k = NULL, funct = "step")
  } else {
    decay <- list(L = decayVals$L, k = decayVals$k, funct = decayVals$funct)
  }

  ## Generate a list containing the placeholder sequences from the function
  ## arguments.
  ## Get input arguments, remove function name from list and unwanted entries
  funArgs <- as.list(match.call()[-1])
  funArgs <- funArgs[!(names(funArgs) %in% c("baseList"))]
  ## Function arguments are unevaluated and can contain calls and symbols. Thus,
  ## we need to evaluate them before in the parent environment.
  for (arg in names(funArgs)) {
    funArgs[[arg]] <- eval(funArgs[[arg]], envir = parent.frame())
  }
  ## Generate list
  placeholderseq <- .placeholderseqGen(
    x = funArgs,
    placeholders = c("coverage")
  )

  ## Generate date sequence, if NULL
  if (is.null(dates)) {
    dates <- xmlTimeGen(
      startDate = startDate,
      endDate = endDate,
      interval = interval
    )
  }

  ## Check if the number of mosquitos is equal or bigger than the longest
  ## placeholder sequence.
  placeholderseq <- .equalizePlaceholders(mosquitos,
    placeholderseq = placeholderseq
  )

  ## Add information
  outlist <- list()
  outlist <- .xmlAddList(
    data = outlist, sublist = NULL,
    entry = NULL,
    input = list(
      name = component
    )
  )

  ## Loop over mosquitoes and add information to list
  for (i in seq_len(length(mosquitos))) {
    outlist <- .xmlAddList(
      data = outlist, sublist = "description",
      entry = "anopheles",
      input = list(
        mosquito = mosquitos[i],
        seekingDeathRateIncrease = list(
          initial = 0.0,
          decay = list(
            L = 0.2466,
            "function" = "step"
          )
        ),
        probDeathOvipositing = list(
          initial = 0.0,
          decay = list(
            L = 0.2466,
            "function" = "step"
          )
        ),
        emergenceReduction = list(
          initial = if (!is.null(placeholderseq[["coverage"]])) {
            placeholderseq[["coverage"]][[i]]
          } else {
            coverage
          },
          decay = append(
            list(
              "function" = decay[["funct"]],
              L = decay[["L"]]
            ),
            if (!is.null(decay[["k"]])) {
              list(k = decay[["k"]])
            }
          )
        )
      )
    )
  }

  ## Add deployments
  for (i in seq_len(length(dates))) {
    temp <- list(
      deploy = list(
        time = dates[[i]]
      )
    )

    outlist <- .xmlAddList(
      data = outlist, sublist = c("timed"),
      entry = NULL,
      input = temp
    )
  }

  ## Make sure interventions header is set
  baseList <- .defineInterventionsHeader(baseList = baseList)

  ## Add to base list
  baseList <- .xmlAddList(
    data = baseList, sublist = c("interventions", "vectorPop"),
    entry = "intervention", input = outlist
  )

  return(baseList)
}

##' @rdname defineLarv
##' @export
define_larv <- defineLarv

## DEPRECATED
##' @title Function that writes the larviciding component xml chunk
##' @param baseList List with experiment data.
##' @param mosqs Names of mosquito species to simulate
##' @param component Name of vector population intervention (i.e. LSM)
##' @param coverage A variable or a numeric value
##' @param decayVals Decay (if not specified, constant effectiveness, step
##'   function as default)
##' @param y1 Year of the first date (surveys starting from year y1)
##' @param m1 Month of the first date
##' @param y2 Year of the end date (surveys continuing until year y2)
##' @param m2 Month of the end date
##' @param interval Month, year, week, quarter
##' @param every Integer value
##' @param SIMSTART Starting date of the simulations in the format "yyyy-mm-dd",
##'   default e.g. "1918-01-01"
##' @export
define_larv_compat <- function(baseList, mosqs, component = "LSM",
                               coverage = "@futLSMcov@",
                               decayVals = list(
                                 L = 0.25, k = NULL, funct = "step"
                               ),
                               y1 = 2018, y2 = 2030, m1 = 4, m2 = 6, every = 2,
                               interval = "week", SIMSTART = "1918-01-01") {
  dates <- .deployTime_compat(
    y1 = y1, y2 = y2, m1 = m1, m2 = m2, d1 = 5, d2 = 5, every = every,
    interval = interval
  )

  baseList <- defineLarv(
    baseList = baseList, mosquitos = mosqs, component = component,
    coverage = coverage, decayVals = decayVals, dates = dates
  )

  return(baseList)
}


##' @title Writes importation intervention
##' @description Models importation of P. falciparum infections directly into
##'   humans from an external source. This is infections, not inoculations or
##'   EIR being imported.
##' @param baseList List with experiment data.
##' @param name Name of the intervention.
##' @param value Number of imported infections per 1000.
##' @param time Rate of importation, if 0, constant importation rate since the
##'   beginning.
##' @export
defineImportedInfections <- function(baseList, name = "importedInfections",
                                     value = 10, time = 0) {

  ## Make sure interventions header is set
  baseList <- .defineInterventionsHeader(baseList = baseList)

  ## Add to base list
  baseList <- .xmlAddList(
    data = baseList, sublist = c("interventions"),
    entry = "importedInfections",
    input = list(
      name = name,
      timed = list(
        rate = list(
          value = value,
          time = time
        )
      )
    )
  )

  return(baseList)
}

##' @rdname defineImportedInfections
##' @export
define_importedInfections <- defineImportedInfections

## DEPRECATED
##' @title Writes importation intervention
##' @param baseList List with experiment data.
##' @param val Number of imported infections per 1000.
##' @param time Rate of importation, if 0, constant importation rate since the
##'   beginning.
##' @export
define_importedInfections_compat <- function(baseList, val = 10, time = 0) {
  baseList <- defineImportedInfections(
    baseList,
    name = "importedInfections",
    value = val, time = time
  )

  return(baseList)
}
