## https://swisstph.github.io/openmalaria/schema-43.html#elt-component-2
## https://swisstph.github.io/openmalaria/schema-43.html#elt-GVI




#' Adds vaccine intervention parameterisation to baseXMLfile
#'@param baseXMLfile xml file to which this functions appends to
#'@param vaccine parameterization list (see example below) 
#'@param append if T, then append to existing baseXMLfile, otherwise overwrites 
#'@param name name tag list 
#'@param hist if T, then decay is assumed to be step function set to 1 for a year and then to zero for the remainder TODO

define_vaccine<-function(baseXMLfile,
                         vaccine_parameterization,
                         append=T,
                         name=NULL,
                         hist=F){
  ## Examples
  ## vaccine_parameterization=list(RTSS_Vaccine=list(mode_of_action="PEV",decay=list(L="223d",`function`="weibull"),efficacyB=list(value="0.91"),initialEfficacy=list(value="0.91"))), from Penny et al. 2015 
  
  # Verify input
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertSubset(hist,
                          choices = c(T, F),
                          add = assertCol
  )
  checkmate::reportAssertions(assertCol)
  
  for (k in names(vaccine_parameterization)){
    
    print(paste0("Defining ",k,"_intervention_cohort first..."))
    baseXMLfile <- .xmlAddList(
      data = baseXMLfile, sublist = c("interventions", "human"),append=append,
      entry = "component",    
      input = list(id=paste0(k,"_intervention_cohort"),
                   recruitmentOnly=list(),
                   subPopRemoval=list(afterYears="5"))
    )
    
    print(paste0("Defining ",k," parameterization..."))
    componentData<-vaccine_parameterization[[k]]
    
    baseXMLfile <- .xmlAddList(
      data = baseXMLfile, sublist = c("interventions", "human"),append=append,
      entry = "component",
      input = list(id=k,
                   name=k,
                   setNames(list(list(decay=componentData[["decay"]],
                                      efficacyB=componentData[["efficacyB"]],
                                      initialEfficacy=componentData[["initialEfficacy"]])),
                            componentData[["mode_of_action"]])
      )
    )
    
  }
  
  return(baseXMLfile)
}




#' Adds vector control intervention parameterisation to baseXMLfile
#'@param baseXMLfile xml file to which this functions appends to
#'@param intervention_parameterization vector control intervention parameterization list depending on three parameters (deterrency, preprandrial, postprandial) and decay functions: 
#'@param append if T, then append to existing baseXMLfile, otherwise overwrites 
#'@param name name tag list 
#'@param hist if T, then decay is assumed to be step function set to 1 for a year and then to zero for the remainder
#'@param resistance scaling function of insecticide resistance ##TODO

define_vector_control<-function(baseXMLfile,
                                intervention_parameterization,
                                append=T,
                                name=NULL,
                                hist=F,
                                resistance=0.1){
  ## Examples
  ## intervention_parameterization=list("LLIN"=list("deterrency_snippet"=list("anophelesParams"=list("mosquito"="Anopheles gambiae", "propActive"=1),"decay"=list("L"=8.826,"function"="weibull","k"=0.6893),"deterrency"=list("value"=0.73))))  
  ## name=list("LLIN"="your LLIN tag")
  
  
  # Verify input
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertSubset(hist,
                          choices = c(T, F),
                          add = assertCol
  )
  checkmate::assertNumeric(resistance, lower = 0, upper=1, null.ok = TRUE, add = assertCol)
  checkmate::reportAssertions(assertCol)
  
  #some checks
  if(is.null(baseXMLfile$interventions$human)){stop("To append, the baseXMLfile needs a child called '$interventions$human'")}
  mosquito_GVI_snippets<-unique(sapply(intervention_parameterization,function(x) x$anophelesParams$mosquito))
  #if(!mosquito_GVI_snippets%in%baseXMLfile$entomology$vector$anopheles$mosquito){stop("To append, the component mosquito must be one of those specified in the entomology part of the baseXMLfile.")}
  
  for (k in names(intervention_parameterization)){
    
    componentData<-intervention_parameterization[[k]]
    
    for (effects in names(componentData)){
      
      component_id<-paste0(k,"-",effects)
      print(paste0("Defining intervention with component_id ",component_id))
      
      ## Add decay and effect information
      baseXMLfile <- .xmlAddList(
        data = baseXMLfile, sublist = c("interventions", "human"),append=append,
        entry = "component",
        input = list(
          id = component_id,
          name = if (is.null(name)) "your_tag" else name[[k]],
          GVI = list(
            decay = if (hist) list("L"=1,"function"="step") else componentData[[effects]][["decay"]],
            anophelesParams = list(
              mosquito = componentData[[effects]][["anophelesParams"]][["mosquito"]],
              propActive = componentData[[effects]][["anophelesParams"]][["propActive"]],
              deterrency = componentData[[effects]][["deterrency"]],
              preprandialKillingEffect = componentData[[effects]][["preprandialKillingEffect"]],
              postprandialKillingEffect = componentData[[effects]][["postprandialKillingEffect"]]
            )
          )
        )
      )
      
    }
  }
  
  return(baseXMLfile)
}








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
  ## If 'component' is 'nothing', set propActive to zero
  if (component == "nothing") {
    propActive <- rep(0, length(mosquitos))
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
##' @param experiment List with experiment data.
##' @param component Name of the intervention, can be any name but needs to be
##'   the same as defined in deployment
##' @param durationBlood Clearance of blood stage parasites
##' @param durationLiver Clearance of liver stage parasites
##' @export
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

##' @title Writes an intervention parameterisation xml chunk that does nothing
##' @description This is useful if something needs to be deployed, as a
##'   placeholder.
##' @param experiment List with experiment data.
##' @param mosquitos Name of mosquito species affected by the intervention.
##' @param component Name of the intervention, can be any name but needs to be
##'   the same as defined in deployment
##' @export
defineNothing <- function(experiment, mosquitos) {

  ## This is simply a subset of defineIRS
  experiment <- defineIRS(
    experiment = experiment, mosquitos = mosquitos,
    component = "nothing", noeffect = NULL
  )

  return(experiment)
}

##' @rdname defineNothing
##' @export
define_nothing <- defineNothing

##' @rdname defineNothing
##' @export
define_nothing_compat <- function(experiment, mosquitos, component = "nothing") {

  ## This is simply a subset of defineIRS
  experiment <- defineIRS(
    experiment = experiment, mosquitos = mosquitos,
    component = "nothing", noeffect = NULL
  )

  return(experiment)
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
##' @param experiment List with experiment data.
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
defineITN <- function(experiment, component = "histITN", noeffect = "outdoor", mosquitos,
                      halflife = 2, resist = TRUE, historical = FALSE,
                      strong = FALSE) {
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

  ## Add data to experiment
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
        initialsecticide = list(
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

  experiment <- .xmlAddList(
    data = experiment, sublist = c("interventions", "human"),
    entry = "component", input = outlist
  )
  return(experiment)
}

##' @rdname defineITN
##' @export
define_ITN <- defineITN

##' @rdname defineITN
##' @export
define_ITN_compat <- defineITN
