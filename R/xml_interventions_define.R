## https://swisstph.github.io/openmalaria/schema-43.html#elt-component-2
## https://swisstph.github.io/openmalaria/schema-43.html#elt-GVI

##' @title Adds the IRS intervention parameterisation
##' @param experiment List with experiment data.
##' @param mosquitos Mosquito species affected by the intervention.
##' @param component Insecticde to use. This will automatically specify efficacy
##'   and effect duration. Needs to from predefined choices.
##' @param noeffect Pattern which defines which mosquitoes are unaffected by
##'   intervention. Can be "outdoor", "indoor" or NULL.
##' @param steplife Step function decay, the number of months it's effective.
##' @export
defineIRS <- function(experiment, mosquitos, component = c(
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
  experiment <- .xmlAddList(
    data = experiment, sublist = c("interventions", "human"),
    entry = "component",
    input = list(
      id = component,
      name = component,
      GVI = list(
        decay = componentData[[component]][["decayL"]],
        "function" = componentData[[component]][["function"]]
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

  ## Add mosquito information
  for (i in seq_len(length(mosquitos))) {
    experiment <- .xmlAddList(
      data = experiment, sublist = c(
        "interventions", "human", "component", "GVI"
      ),
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
  return(experiment)
}

##' @rdname defineIRS
##' @export
define_IRS <- defineIRS

##' @rdname defineIRS
##' @export
define_IRS_compat <- defineIRS


## https://swisstph.github.io/openmalaria/schema-43.html#elt-treatSimple-3

##' @title SMC, MDA parameterization section
##' @description Writes the treatSimple intervention used for mass treatments
##'   (i.e. MDA, SMC)
##' @param component Name of the intervention, can be any name but needs to be
##'   the same as defined in deployment
##' @param durationBlood Clearance of blood stage parasites
##' @param durationLiver Clearance of liver stage parasites
##' @export
##' @examples
##' # SMC intervention with a 30 day effect
##' define_treatSimple( component = "SMC", durationBlood = "30d")
##' # deploying SMC from March to May, in children up to age 10
##' deploy_it( component = "SMC", y1 = 2005, y2 = 2006, every = 1,
##' interval = "month"
##' , maxAge = 10, minAge = .5, m1 = 3, m2 = 5)
defineTreatSimple <- function(experiment, component = "MDA",
                              durationBlood = "15d", durationLiver = 0) {
  # Verify input
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(component, add = assertCol)
  checkmate::assert(
    checkmate::checkCharacter(durationBlood, pattern = "[0-9]+d{1}\\b"),
    checkmate::checkNumber(durationBlood, lower = -1),
    add = assertCol
  )
  checkmate::assert(
    checkmate::checkCharacter(durationLiver, pattern = "[0-9]+d{1}\\b"),
    checkmate::checkNumber(durationLiver, lower = -1),
    add = assertCol
  )
  checkmate::reportAssertions(assertCol)

  ## Add information
  experiment <- .xmlAddList(
    data = experiment, sublist = c("interventions", "human"),
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

  return(experiment)
}

##' @rdname defineTreatSimple
##' @export
define_treatSimple <- defineTreatSimple

##' @rdname defineTreatSimple
##' @export
define_treatSimple_compat <- defineTreatSimple
