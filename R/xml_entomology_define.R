##' Defines entomology setting and writes it baseList file
##' @param baseList List with experiment data.
##' @param seasonalityParameters Provides seasonality parameter list
##' @param mosquitoParameters Mosquito bionomics parameterization list of
##'   different mosquito species as obtained from AnophelesModel package
##'   function get_OM_ento_snippet or manually
##' @param verbatim If TRUE, the show messages
##' @param append If TRUE, then append to existing baseList, otherwise overwrite
##' @examples  MosquitoParameters=list("Anopheles gambiae"=list(
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
defineEntomology <- function(baseList, seasonalityParameters, mosquitoParameters, 
                             verbatim= FALSE,append = TRUE) {


  ## Verify input
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertList(mosquitoParameters)
  checkmate::assertList(seasonalityParameters)
  checkmate::assertSubset(verbatim,
                          choices = c(TRUE, FALSE),
                          add = assertCol
  )
  checkmate::assertSubset(append,
                          choices = c(TRUE, FALSE),
                          add = assertCol
  )
  checkmate::reportAssertions(assertCol)
  
  
  for (k in names(mosquitoParameters)) {
    if(verbatim){message(paste0("Writing mosquito bionomics and seasonality data for vector species ",k," to baseXML file..."))}
    mosqData <- mosquitoParameters[[k]]
    seasData <- seasonalityParameters[[k]]

    ## Check names
    if (!(seasData$seasonality=="fourierSeries" && length(seasData$seasonality)==1)&&
        !(is.numeric(seasData$seasonality)&&length(seasData$seasonality)==12)&&
        !(grepl("^@.*@",seasData$seasonality)&&length(seasData$seasonality)==12)&&
        !(is.numeric(seasData$seasonality)&&length(seasData$seasonality)==365)&&
        !!(grepl("^@.*@",seasData$seasonality)&&length(seasData$seasonality)==365)){
      stop("Seasonality needs to be either 'fourierSeries', a numeric vector or 
           a vector of placeholders '@foo@' of length 12 or 365")
    }
    
    inputList<-list(
      mosquito = k,
      propInfected = seasData$propInfected,
      propInfectious = seasData$propInfectious)
    
    
    ## Add seasonality part
    if(length(seasData$seasonality)==1){
      inputList<-append(inputList,
                        list(seasonality=list(
                          annualEIR=seasData$annualEIR,
                          input="EIR",
                          fourierSeries=list(EIRRotateAngle = "0",
                                           coeffic = list(a = "0.8968", b = "2.678"),
                                           coeffic = list(a = "-0.4551", b = "2.599"))))
                        )
    }
    
    if(length(seasData$seasonality)==12){
      
      tmp <- list()
      tmp <- .xmlAddList(
        data = tmp, sublist = NULL,
        entry = NULL,
        input = list(
          smoothing="fourier"
        )
      )
      
      for (mo in seasData$seasonality) {
        tmp <- .xmlAddList(
        data = tmp, sublist = NULL,
        entry = "value",
        input = list(mo)
      )
      }

      
      inputList <- .xmlAddList(data = inputList,sublist="seasonality",entry=NULL,
                               input=list(
                                 annualEIR=seasData$annualEIR,
                                 input="EIR",
                                 monthlyValues=tmp))

    }
    
    if(length(seasData$seasonality)==365){
      
      tmp <- list()
      tmp <- .xmlAddList(
        data = tmp, sublist = NULL,
        entry = NULL,
        input = list(
          smoothing="fourier"
        )
      )
      
      for (mo in seasData$seasonality) {
        tmp <- .xmlAddList(
          data = tmp, sublist = NULL,
          entry = "value",
          input = list(mo)
        )
      }
      
      
      inputList <- .xmlAddList(data = inputList,sublist="seasonality",entry=NULL,
                               input=list(
                                 annualEIR=seasData$annualEIR,
                                 input="EIR",
                                 dailyValues=tmp))
      
    }
    
    ## Add mosq part
    inputList <- .xmlAddList(data = inputList,sublist=NULL,entry="mosq",
                             input = list(
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
    inputList <- .xmlAddList(data = inputList,sublist=NULL,entry="nonHumanHosts",
                             input = list(
                          name = "unprotectedAnimals",
                          mosqRelativeEntoAvailability = list(value = "1.0"),
                          mosqProbBiting = list(value = "0.95"),
                          mosqProbFindRestSite = list(value = "0.95"),
                          mosqProbResting = list(value = "0.99")
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
    input = list(name = "unprotectedAnimals", number = "1.0")
  )

  return(baseList)
}
  
##' @rdname defineEntomology
##' @export
define_entomology <- defineEntomology
