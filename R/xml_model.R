## DEPRECATED
##' @title Write the underlying transmission dynamics
##' @param baseList List with experiment data.
##' @param seed Number of replicates per scenario.
##' @param modelname Select model (default= base 'R0000'). Other options
##'   currently not available.
##' @param selectedOptions Model options to activate. Can be any combination of
##'   c("COMORB_HET", "VECTOR_SIMPLE_MPD_MODEL", "LOGNORMAL_MASS_ACTION",
##'   "MAX_DENS_CORRECTION", "INNATE_MAX_DENS", "INDIRECT_MORTALITY_FIX").
##' @param comorb Comorbidity.
##' @param critage Critical age.
##' @param logodds Log odds terms.
##' @param healthSystemMemory healthSystemMemory (see OpenMalaria).
##' @param ... deprecated
##' @export
write_end_compat <- function(baseList,
                             seed = 4,
                             modelname = "base",
                             selectedOptions = NULL,
                             comorb = 0.0968,
                             critage = 0.117383,
                             logodds = 0.736202,
                             healthSystemMemory = "6t", ...) {
  if (!is.null(selectedOptions) == TRUE) {
    modelOptions <- list(
      COMORB_HET = FALSE,
      VECTOR_SIMPLE_MPD_MODEL = FALSE,
      LOGNORMAL_MASS_ACTION = FALSE,
      MAX_DENS_CORRECTION = FALSE,
      INNATE_MAX_DENS = FALSE,
      INDIRECT_MORTALITY_FIX = FALSE
    )

    modelOptions[names(modelOptions) %in% selectedOptions] <- TRUE
    modelOptions <- lapply(modelOptions, tolower)

    ModelOptions <- list(
      option = list(name = "VECTOR_SIMPLE_MPD_MODEL", value = modelOptions$VECTOR_SIMPLE_MPD_MODEL),
      option = list(name = "COMORB_HET", value = modelOptions$COMORB_HET),
      option = list(name = "LOGNORMAL_MASS_ACTION", value = modelOptions$LOGNORMAL_MASS_ACTION),
      option = list(name = "MAX_DENS_CORRECTION", value = modelOptions$MAX_DENS_CORRECTION),
      option = list(name = "INNATE_MAX_DENS", value = modelOptions$INNATE_MAX_DENS),
      option = list(name = "INDIRECT_MORTALITY_FIX", value = modelOptions$INDIRECT_MORTALITY_FIX)
    )
  } else {
    ModelOptions <- list(
      option = list(name = "INNATE_MAX_DENS", value = "false"),
      option = list(name = "INDIRECT_MORTALITY_FIX", value = "false")
    )
  }

  baseList <- .xmlAddList(
    data = baseList,
    sublist = "model",
    entry = NULL,
    input = list(
      ModelOptions = ModelOptions,
      clinical = list(
        healthSystemMemory = healthSystemMemory
      ),
      human = list(
        availabilityToMosquitoes = list(
          group = list(lowerbound = "0.0", value = "0.225940909648"),
          group = list(lowerbound = "1.0", value = "0.286173633441"),
          group = list(lowerbound = "2.0", value = "0.336898395722"),
          group = list(lowerbound = "3.0", value = "0.370989854675"),
          group = list(lowerbound = "4.0", value = "0.403114915112"),
          group = list(lowerbound = "5.0", value = "0.442585112522"),
          group = list(lowerbound = "6.0", value = "0.473839351511"),
          group = list(lowerbound = "7.0", value = "0.512630464378"),
          group = list(lowerbound = "8.0", value = "0.54487872702"),
          group = list(lowerbound = "9.0", value = "0.581527755812"),
          group = list(lowerbound = "10.0", value = "0.630257580698"),
          group = list(lowerbound = "11.0", value = "0.663063362714"),
          group = list(lowerbound = "12.0", value = "0.702417432755"),
          group = list(lowerbound = "13.0", value = "0.734605377277"),
          group = list(lowerbound = "14.0", value = "0.788908765653"),
          group = list(lowerbound = "15.0", value = "0.839587932303"),
          group = list(lowerbound = "20.0", value = "1.0")
        )
      ),
      parameters = list(
        interval = "5", iseed = seed, latentp = "3",
        parameter = list(include = "false", name = "'-ln(1-Sinf)'", number = "1", value = "0.050736"),
        parameter = list(include = "false", name = "Estar", number = "2", value = "0.03247"),
        parameter = list(include = "false", name = "Simm", number = "3", value = "0.138161050830301"),
        parameter = list(include = "false", name = "Xstar_p", number = "4", value = "1514.385853233699891"),
        parameter = list(include = "false", name = "gamma_p", number = "5", value = "2.03692533424484"),
        parameter = list(include = "false", name = "sigma2i", number = "6", value = "10.173598698525799"),
        parameter = list(include = "false", name = "CumulativeYstar", number = "7", value = "35158523.31132510304451"),
        parameter = list(include = "false", name = "CumulativeHstar", number = "8", value = "97.334652723897705"),
        parameter = list(include = "false", name = "'-ln(1-alpha_m)'", number = "9", value = "2.33031045876193"),
        parameter = list(include = "false", name = "decay_m", number = "10", value = "2.53106547375805"),
        parameter = list(include = "false", name = "sigma2_0", number = "11", value = "0.655747311168152"),
        parameter = list(include = "false", name = "Xstar_v", number = "12", value = "0.916181104713054"),
        parameter = list(include = "false", name = "Ystar2", number = "13", value = "6502.26335600001039"),
        parameter = list(include = "false", name = "alpha", number = "14", value = "142601.912520000012591"),
        parameter = list(include = "false", name = "Density bias (non Garki)", number = "15", value = "0.177378570987455"),
        parameter = list(include = "false", name = "        sigma2        ", number = "16", value = "0.05"),
        parameter = list(include = "false", name = "log oddsr CF community", number = "17", value = logodds),
        parameter = list(include = "false", name = "Indirect risk cofactor", number = "18", value = "0.018777338"),
        parameter = list(include = "false", name = "Non-malaria infant mortality", number = "19", value = "49.539046599999999"),
        parameter = list(include = "false", name = "Density bias (Garki)", number = "20", value = "4.79610772546704"),
        parameter = list(include = "false", name = "Severe Malaria Threshhold", number = "21", value = "784455.599999999976717"),
        parameter = list(include = "false", name = "Immunity Penalty", number = "22", value = "1"),
        parameter = list(include = "false", name = "Immune effector decay", number = "23", value = "0"),
        parameter = list(include = "false", name = "comorbidity intercept", number = "24", value = comorb),
        parameter = list(include = "false", name = "Ystar half life", number = "25", value = "0.275437402"),
        parameter = list(include = "false", name = "Ystar1", number = "26", value = "0.596539864"),
        parameter = list(include = "false", name = "Asexual immunity decay", number = "27", value = "0"),
        parameter = list(include = "false", name = "Ystar0", number = "28", value = "296.302437899999973"),
        parameter = list(include = "false", name = "Idete multiplier", number = "29", value = "2.797523626"),
        parameter = list(include = "false", name = "critical age for comorbidity", number = "30", value = critage)
      )
    )
  )

  return(baseList)
}
