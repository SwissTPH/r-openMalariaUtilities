## Basic skeleton
baseList <- list(
  ## Mandatory
  expName = "example",
  ## Mandatory
  OMVersion = 44L,
  ## Optional
  ## analysisNo = 1L,
  ## Mandatory
  demography = list(),
  monitoring = list(),
  interventions = list(),
  healthSystem = list(),
  entomology = list(),
  ## These are optional for OM
  ## parasiteGenetics = list(),
  ## pharmacology = list(),
  ## diagnostics = list(),
  model = list()
)

demo_data <- data.frame(
  poppercent = c(
    3.474714994, 12.76004028, 14.52151394, 12.75565434, 10.83632374,
    8.393312454, 7.001421452, 5.800587654, 5.102136612, 4.182561874,
    3.339409351, 2.986112356, 2.555766582, 2.332763433, 1.77400255,
    1.008525491, 0.74167341, 0.271863401, 0.161614642
  ),
  upperbound = c(
    1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90
  )
)

baseList <- defineDemography(
  baseList,
  name = "Ifakara",
  popSize = 4000L,
  maximumAgeYrs = 90,
  lowerbound = 0,
  poppercent = demo_data$poppercent,
  upperbound = demo_data$upperbound
)

baseList[["monitoring"]] <- list(
  name = "Quarterly Surveys",
  ## Mandatory, different from OM schema
  startDate = "2000-01-01",
  continuous = monitoringContinuousGen(
    period = 1,
    options = list(
      name = c("input EIR", "simulated EIR", "human infectiousness", "N_v0",
               "immunity h", "immunity Y", "new infections",
               "num transmitting humans", "ITN coverage", "GVI coverage", "alpha",
               "P_B", "P_C*P_D"),
      value = c("true", "true", "true", "true", "true", "true", "true", "true",
                "true", "true", "true", "false", "false")
    )
  ),
  SurveyOptions = monitoringSurveyOptionsGen(
    options = list(
      name = c("nHost", "nPatent", "nUncomp", "nSevere", "nDirDeaths",
               "inputEIR", "simulatedEIR"),
      value = c("true", "true", "true", "true", "false", "true", "true")
    )
  )
)

baseList[["monitoring"]] <- c(
  baseList[["monitoring"]],
  list(
    surveys =  monitoringSurveyTimesGen(
      detectionLimit = 40, startDate = "2000-01-01", endDate = "2020-01-01",
      interval = "quarterly"
    ),
    ageGroup = surveyAgeGroupsGen(
      lowerbound = 0,
      upperbounds = c(5, 90)
    )
  )
)

baseList[["interventions"]] <- list(
  name = "test",
  human = list(
    component = list(
      id = "GVI",
      name = "DDT test",
      GVI = list(
        decay = list(
          L = "0.5",
          "function" = "exponential"
        ),
        anophelesParams = list(
          mosquito = "gambiae_ss", propActive = "1",
          deterrency = list(value = "0.56"),
          preprandialKillingEffect = list(value = "0"),
          postprandialKillingEffect = list(value = "0.24")
        )
      )
    ),
    deployment = list(
      name = "DDT test",
      component = list(id = "GVI"),
      timed = list(
        deploy = list(coverage = "0.9", time = "10y")
      )
    )
  )
)

baseList[["healthSystem"]] <- list(
  ImmediateOutcomes = list(
    name = "Tanzania ACT",
    drugRegimen = list(
      firstLine = "ACT",
      inpatient = "QN",
      secondLine = "ACT"
    ),
    initialACR = list(
      ACT = list(value = 0.85),
      QN = list(value = 0.998),
      selfTreatment = list(value = 0.63)
    ),
    compliance = list(
      ACT = list(value = 0.9),
      selfTreatment = list(value = 0.85)
    ),
    nonCompliersEffective = list(
      ACT = list(value = 0),
      selfTreatment = list(value = 0)
    ),
    treatmentActions = list(
      ACT = list(
        name = "clear blood-stage infections",
        clearInfections = list(
          stage = "blood",
          timesteps = "1"
        )
      ),
      QN = list(
        name = "clear blood-stage infections",
        clearInfections = list(
          stage = "blood",
          timesteps = "1"
        )
      )
    ),
    pSeekOfficialCareUncomplicated1 = list(value = 0.04),
    pSelfTreatUncomplicated = list(value = 0.01),
    pSeekOfficialCareUncomplicated2 = list(value = 0.04),
    pSeekOfficialCareSevere = list(value = 0.48)
  ),
  CFR = list(
    group = list(lowerbound = 0, value = 0.09189),
    group = list(lowerbound = 0.25, value = 0.0810811),
    group = list(lowerbound = 0.75, value = 0.0648649),
    group = list(lowerbound = 1.5, value = 0.0689189),
    group = list(lowerbound = 2.5, value = 0.0675676),
    group = list(lowerbound = 3.5, value = 0.0297297),
    group = list(lowerbound = 4.5, value = 0.0459459),
    group = list(lowerbound = 7.5, value = 0.0945946),
    group = list(lowerbound = 12.5, value = 0.1243243),
    group = list(lowerbound = 15, value = 0.1378378)
  ),
  ## Mandatory
  pSequelaeInpatient = list(
    interpolation = "none",
    group = list(lowerbound = "0.0", value = 0.0132),
    group = list(lowerbound = "5.0", value = 0.005)
  )
)

baseList[["entomology"]] <- list(
  mode = "dynamic", name = "Namawala",
  vector = list(
    anopheles = list(
      mosquito = "gambiae_ss", propInfected = 0.078, propInfectious = "0.021",
      seasonality = list(
        annualEIR = "16", input = "EIR",
        fourierSeries = list(
          EIRRotateAngle = "0",
          coeffic = list(
            a = "0.8968", b = "2.678"
          ),
          coeffic = list(a = "-0.4551", b = "2.599")
        )
      ),
      mosq = list(
        minInfectedThreshold = "0.001",
        mosqRestDuration = list(value = "3"),
        extrinsicIncubationPeriod = list(value = "11"),
        mosqLaidEggsSameDayProportion = list(value = "0.313"),
        mosqSeekingDuration = list(value = "0.33"),
        mosqSurvivalFeedingCycleProbability = list(value = "0.623"),
        availability = list(distr = "const"),
        mosqProbBiting = list(mean = "0.95", variance = "0"),
        mosqProbFindRestSite = list(mean = "0.95", variance = "0"),
        mosqProbResting = list(mean = "0.99", variance = "0"),
        mosqProbOvipositing = list(value = "0.88"),
        mosqHumanBloodIndex = list(value = "0.939")
      ),
      nonHumanHosts = list(
        name = "unprotectedAnimals",
        mosqRelativeEntoAvailability = list(value = "1.0"),
        mosqProbBiting = list(value = "0.95"),
        mosqProbFindRestSite = list(value = "0.95"),
        mosqProbResting = list(value = "0.99")
      )
    ),
    nonHumanHosts = list(name = "unprotectedAnimals", number = "1.0")
  )
)

baseList[["model"]] <- list(
  ModelOptions = list(
    option = list(name = "LOGNORMAL_MASS_ACTION", value = "true"),
    option = list(name = "NO_PRE_ERYTHROCYTIC", value = "true"),
    option = list(name = "INNATE_MAX_DENS", value = "false"),
    option = list(name = "INDIRECT_MORTALITY_FIX", value = "false"),
    option = list(name = "NON_MALARIA_FEVERS", value = "true")
  ),
  clinical = list(
    healthSystemMemory = "6",
    NonMalariaFevers = list(
      incidence = list(
        group = list(lowerbound = "0", value = "0.322769924518357"),
        group = list(lowerbound = "0", value = "0.308520194304172"),
        group = list(lowerbound = "1", value = "0.279441774808493"),
        group = list(lowerbound = "2", value = "0.250431781111273"),
        group = list(lowerbound = "3", value = "0.223285859756841"),
        group = list(lowerbound = "4", value = "0.199298352451799"),
        group = list(lowerbound = "5", value = "0.179376872365614"),
        group = list(lowerbound = "6", value = "0.163623659390782"),
        group = list(lowerbound = "7", value = "0.152227726923469"),
        group = list(lowerbound = "8", value = "0.145022785567758"),
        group = list(lowerbound = "9", value = "0.141493087461765"),
        group = list(lowerbound = "10", value = "0.140473293219353"),
        group = list(lowerbound = "11", value = "0.141109775159515"),
        group = list(lowerbound = "12", value = "0.142644475217328"),
        group = list(lowerbound = "13", value = "0.144335079395766"),
        group = list(lowerbound = "14", value = "0.145964032924869"),
        group = list(lowerbound = "15", value = "0.147708915135714"),
        group = list(lowerbound = "16", value = "0.149731543445568"),
        group = list(lowerbound = "17", value = "0.151887428568276"),
        group = list(lowerbound = "18", value = "0.154060663485195"),
        group = list(lowerbound = "19", value = "0.156179169710494"),
        group = list(lowerbound = "20", value = "0.158135015380583"),
        group = list(lowerbound = "21", value = "0.159704766482219"),
        group = list(lowerbound = "22", value = "0.160807788387655"),
        group = list(lowerbound = "23", value = "0.161427976448279"),
        group = list(lowerbound = "24", value = "0.161620429119137"),
        group = list(lowerbound = "25", value = "0.16144021875986"),
        group = list(lowerbound = "26", value = "0.160943264630612"),
        group = list(lowerbound = "27", value = "0.160217573697398"),
        group = list(lowerbound = "28", value = "0.159422614374451"),
        group = list(lowerbound = "29", value = "0.158542519631641"),
        group = list(lowerbound = "30", value = "0.157501217628248"),
        group = list(lowerbound = "31", value = "0.156175160594841"),
        group = list(lowerbound = "32", value = "0.154402302191411"),
        group = list(lowerbound = "33", value = "0.152102040636481"),
        group = list(lowerbound = "34", value = "0.14921450014676"),
        group = list(lowerbound = "35", value = "0.145714433541659"),
        group = list(lowerbound = "36", value = "0.141800502067518"),
        group = list(lowerbound = "37", value = "0.137916853907569"),
        group = list(lowerbound = "38", value = "0.134503529382102"),
        group = list(lowerbound = "39", value = "0.131746276580642"),
        group = list(lowerbound = "40", value = "0.12969902537497"),
        group = list(lowerbound = "41", value = "0.128398077347679"),
        group = list(lowerbound = "42", value = "0.127864136551891"),
        group = list(lowerbound = "43", value = "0.12804497197004"),
        group = list(lowerbound = "44", value = "0.128894055047661"),
        group = list(lowerbound = "45", value = "0.130350838992718"),
        group = list(lowerbound = "46", value = "0.132286605622701"),
        group = list(lowerbound = "47", value = "0.134599921072495"),
        group = list(lowerbound = "48", value = "0.137212726976988"),
        group = list(lowerbound = "49", value = "0.140035253913284"),
        group = list(lowerbound = "50", value = "0.142934573453621"),
        group = list(lowerbound = "51", value = "0.145830221511879"),
        group = list(lowerbound = "52", value = "0.148674810561069"),
        group = list(lowerbound = "53", value = "0.151497963594518"),
        group = list(lowerbound = "54", value = "0.15438856687865"),
        group = list(lowerbound = "55", value = "0.157403790093505"),
        group = list(lowerbound = "56", value = "0.16059513222516"),
        group = list(lowerbound = "57", value = "0.16402433342886"),
        group = list(lowerbound = "58", value = "0.16770481415944"),
        group = list(lowerbound = "59", value = "0.171626873047865"),
        group = list(lowerbound = "60", value = "0.175748327054247"),
        group = list(lowerbound = "61", value = "0.180030857856799"),
        group = list(lowerbound = "62", value = "0.184411365583771"),
        group = list(lowerbound = "63", value = "0.188816421789366"),
        group = list(lowerbound = "64", value = "0.19316997803338"),
        group = list(lowerbound = "65", value = "0.197435603275487"),
        group = list(lowerbound = "66", value = "0.201578808813379"),
        group = list(lowerbound = "67", value = "0.205556806881398"),
        group = list(lowerbound = "68", value = "0.209307183457343"),
        group = list(lowerbound = "69", value = "0.212783260344084"),
        group = list(lowerbound = "70", value = "0.215944154621391"),
        group = list(lowerbound = "71", value = "0.218749275266548"),
        group = list(lowerbound = "72", value = "0.221187990639016"),
        group = list(lowerbound = "73", value = "0.223361260399378"),
        group = list(lowerbound = "74", value = "0.225363436789592"),
        group = list(lowerbound = "75", value = "0.227254280093211"),
        group = list(lowerbound = "76", value = "0.229084576349576"),
        group = list(lowerbound = "77", value = "0.230891971097789"),
        group = list(lowerbound = "78", value = "0.232690225166173"),
        group = list(lowerbound = "79", value = "0.234484973338876"),
        group = list(lowerbound = "80", value = "0.236276361586796"),
        group = list(lowerbound = "81", value = "0.238064394629696"),
        group = list(lowerbound = "82", value = "0.239849077182917"),
        group = list(lowerbound = "83", value = "0.241630413957381"),
        group = list(lowerbound = "84", value = "0.243408409659591"),
        group = list(lowerbound = "85", value = "0.245183068991633"),
        group = list(lowerbound = "86", value = "0.246954396651183"),
        group = list(lowerbound = "87", value = "0.248722397331501"),
        group = list(lowerbound = "88", value = "0.250487075721441"),
        group = list(lowerbound = "89", value = "0.252248436505447"),
        group = list(lowerbound = "90", value = "0.253127874257909")
      )
    )
  ),
  human = list(
    availabilityToMosquitoes = list(
      group = list(lowerbound = "0", value = "0.7076"),
      group = list(lowerbound = "0", value = "0.8538"),
      group = list(lowerbound = "5", value = "1.0"),
      group = list(lowerbound = "5", value = "1.0")
    )
  ),
  parameters = list(
    interval = "5", iseed = "1", latentp = "3",
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
    parameter = list(include = "false", name = "log oddsr CF community", number = "17", value = "0.736202"),
    parameter = list(include = "false", name = "Indirect risk cofactor", number = "18", value = "0.018777338"),
    parameter = list(include = "false", name = "Non-malaria infant mortality", number = "19", value = "49.539046599999999"),
    parameter = list(include = "false", name = "Density bias (Garki)", number = "20", value = "4.79610772546704"),
    parameter = list(include = "false", name = "Severe Malaria Threshhold", number = "21", value = "784455.599999999976717"),
    parameter = list(include = "false", name = "Immunity Penalty", number = "22", value = "1"),
    parameter = list(include = "false", name = "Immune effector decay", number = "23", value = "0"),
    parameter = list(include = "false", name = "comorbidity intercept", number = "24", value = "0.0968"),
    parameter = list(include = "false", name = "Ystar half life", number = "25", value = "0.275437402"),
    parameter = list(include = "false", name = "Ystar1", number = "26", value = "0.596539864"),
    parameter = list(include = "false", name = "Asexual immunity decay", number = "27", value = "0"),
    parameter = list(include = "false", name = "Ystar0", number = "28", value = "296.302437899999973"),
    parameter = list(include = "false", name = "Idete multiplier", number = "29", value = "2.797523626"),
    parameter = list(include = "false", name = "critical age for comorbidity", number = "30", value = "0.117383")
  )
)