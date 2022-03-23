##'  @title Dictionary of survey measure numbers to names 
##' @keywords internal
.numberToSurveyMeasure <- function() {
  dict <- data.table(
    measure_index = as.integer(c(
      0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
      23, 24, 25, 26, 27, 30, 31, 32, 33, 34, 35, 35, 39, 40, 41, 42, 43, 44, 45, 46,
      47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66,
      67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79
    )),
    measure_name = c(
      "nHost", "nInfect", "nExpectd", "nPatent", "sumLogPyrogenThres", "sumlogDens",
      "totalInfs", "nTransmit", "totalPatentInf", "sumPyrogenThresh", "nTreatments1",
      "nTreatments2", "nTreatments3", "nUncomp", "nSevere", "nSeq", "nHospitalDeaths",
      "nIndDeaths", "nDirDeaths", "nEPIVaccinations", "allCauseIMR", "nMassVaccinations",
      "nHospitalRecovs", "nHospitalSeqs", "nIPTDoses", "annAvgK", "nNMFever", "innoculationsPerAgeGroup",
      "Vector_Nv0", "Vector_Nv", "Vector_Ov", "Vector_Sv", "inputEIR", "simulatedEIR", "Clinical_RDTs",
      "Clinical_DrugUsage", "Clinical_FirstDayDeaths", "Clinical_HospitalFirstDayDeaths", "nNewInfections",
      "nMassITNs", "nEPI_ITNs", "nMassIRS", "nMassVA", "Clinical_Microscopy", "Clinical_DrugUsageIV",
      "nAddedToCohort", "nRemovedFromCohort", "nMDAs", "nNmfDeaths", "nAntibioticTreatments", "nMassScreenings", "nMassGVI",
      "nCtsIRS", "nCtsGVI", "nCtsMDA", "nCtsScreenings", "nSubPopRemovalTooOld", "nSubPopRemovalFirstEvent", "nLiverStageTreatments", "nTreatDiagnostics",
      "nMassRecruitOnly", "nCtsRecruitOnly", "nTreatDeployments", "sumAge", "nInfectByGenotype", "nPatentByGenotype", "logDensByGenotype",
      "nHostDrugConcNonZero", "sumLogDrugConcNonZero", "expectedDirectDeaths", "expectedHospitalDeaths", "expectedIndirectDeaths", "expectedSequelae",
      "expectedSevere", "innoculationsPerVector"
    )
  )
  return(dict)
}

##' @title List of measures needed to calculate epidemiological indicators
##' @keywords internal
.measuresNeededForPostprocessing <- function() {
  measuresNeededForPostprocessing<-list(
    prevalence=c("nPatent","nHost"),
    incidence=c("nUncomp","nSevere","nHost"),
    tUncomp=c("nTreatments1","nTreatments2"),
    tSevere=c("nTreatments3"),
    nHosp=c("nHospitalDeaths","nHospitalRecovs","nHospitalSeqs"),
    edeath=c("expectedDirectDeaths","expectedIndirectDeaths","nHost"),
    edirdeath=c("expectedDirectDeaths","nHost"),
    ddeath=c("nIndDeaths","nDirDeaths","nHost")
  )
  return(measuresNeededForPostprocessing)
}


##' @title Read survey output file
##' @param outputFile Survey output data file location
##' @param filterBySurveyMeasure Character vector of survey measures to filter 
##' while reading from file
##' @export
readSurveyOutput <- function(outputFile, filterBySurveyMeasure=NULL) {
  
  ## Verify input
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(
    outputFile, add = assertCol
  )
  checkmate::reportAssertions(assertCol)
  
  ## Read file and filter
  outputDF<-fread(file=outputFile)
  colnames(outputDF)<-c("survey_point","age_group","measure_index","value")
  if(!is.null(filterBySurveyMeasure)){
    if(any(filterBySurveyMeasure%in%.numberToSurveyMeasure()$measure_name==F)){
      stop(paste0("Cannot filter for survey measures ",filterBySurveyMeasure[filterBySurveyMeasure%in%.numberToSurveyMeasure()$measure_name]))
    }else{
    outputDF<-outputDF[measure_index%in%.numberToSurveyMeasure()[measure_name%in%filterBySurveyMeasure]$measure_index]
    }
  }
  
  ## Add file name index
  outputDF <- outputDF[, scenario_file_index := as.numeric(gsub(".*_(\\d+)_out.txt","\\1",basename(outputFile)))]

  ## Translate monitoring
  dict <- .numberToSurveyMeasure()
  outputDF[, measure_name := dict[outputDF, measure_name, on = "measure_index", roll = "nearest"]][, c("measure_index") := NULL]
  
  ## Translate survey number to time, we remove first index since we want (n-1,n]
  timeDF <- data.table(
    survey_point = c(1:length(get(x = "surveyTimes", envir = openMalariaUtilities:::.pkgcache)$timestep)),
    timestep = get(x = "surveyTimes", envir = openMalariaUtilities:::.pkgcache)$timestep,
    survey_date = as.Date(get(x = "surveyTimes", envir = openMalariaUtilities:::.pkgcache)$date)
  )
  ## Join to get survey date, last timeDF has no outputDF index, na produced
  outputDF<-outputDF[timeDF,on="survey_point"][,-c("timestep","survey_point")]%>%na.omit()

  return(outputDF)
}


##' @title Calculate epidemiological indicators from stacked raw data
##' @param rawdata Stacked survey output data file location, by default stackedSurveyOutput.dat file
##' @param metadata Experiment metadata file location, by default scens object from cache
##' @param metadataFeatures Character vector of metadata features to keep, if null keep all
##' @param indicators Character vector of epidemiological indicators to calculate
##' @param keepMeasures Character vector of indicators whose measures you want to keep in the function output, if null keep none
##' @export
calculateEpidemiologicalIndicators<-function(rawdata=NULL,metadata=NULL,
                                             metadataFeatures=NULL,
                                             indicators=c("incidence","prevalence"),
                                             keepMeasuresForIndicators=NULL){
  
  ## Read stacked survey output
  if(is.null(rawdata)){
    rawdata<-file.path(get(x = "postprocessingDir", envir = openMalariaUtilities:::.pkgcache),"stackedSurveyOutput.dat")
    if (file.exists(rawdata) == TRUE) {
      outputs<-read.table(rawdata,sep=",",header=T,stringsAsFactors = T)%>%as.data.table
    } else {
      stop(paste0("File ", rawdata, " not found."))
    }
  }else{
    outputs<-read.table(rawdata,sep=",",header=T,stringsAsFactors = T)%>%as.data.table
  }   
  
  ## Read metadata (aka scens)
  if(is.null(metadata)){
    if (file.exists(file.path(get(x = "cacheDir", envir = openMalariaUtilities:::.pkgcache), "scens.RData")) == TRUE) {
      load(file.path(get(x = "cacheDir", envir = openMalariaUtilities:::.pkgcache), "scens.RData"))
    } else {
        stop(paste0("File ", file.path(get(x = "cacheDir", envir = openMalariaUtilities:::.pkgcache), "scens.RData"), " not found."))
      }
  }else{
    load(metadata)
  }
  metadata<-scens%>%as.data.table;rm(scens)
  output_filenames<-get(x = "outputsDir", envir = openMalariaUtilities:::.pkgcache)%>%list.files(pattern="*_out.txt",full.names=T)
  output_index<-as.numeric(gsub(".*_(\\d+)_out.txt","\\1",output_filenames))
  
  if(length(output_filenames)<nrow(metadata)){
    warning("Fewer output files than scenarios in metadata!")
    warning("We assume that row number in metadata corresponds to output file index!")
    metadata[,scenario_file_index:=1:nrow(metadata)]
    metadata<-merge(metadata,
                    data.frame(file=output_filenames,scenario_file_index=output_index),
                    by="scenario_file_index")
  }else{
    metadata$file<-output_filenames
    metadata$scenario_file_index<-output_index
  }
  
  
  if(!is.null(metadataFeatures)){
    metadataFeatures<-lapply(metadataFeatures,function(x) colnames(metadata)[grepl(x,colnames(metadata))])%>%unlist%>%unique
    if(length(metadataFeatures)==0){
      warning("None of your metadata features or patterns where found. 
              Only default features (scenario_file_index, ageGroups, seed) will be added.")
    }
    metadataFeatures<-unique(c(metadataFeatures,c("scenario_file_index","ageGroups","seed","pop")))
  metadata<-metadata[,..metadataFeatures]
  }
  
  ## Recode age groups
  if("ageGroups"%in%names(metadata)){
  age_group<-strsplit(unique(metadata$ageGroups),"-")[[1]]
  age_group<-as.list(setNames(as.character(0:(length(age_group)-1)),c("NA",paste(head(age_group,-1),tail(age_group,-1),sep="-"))))
  outputs[,age_group:=as.factor(age_group)]
  levels(outputs$age_group)<-age_group
  metadata[,ageGroups:=NULL]
  }else{
    stop("You need a column named 'ageGroups' from monitoring$ageGroup in the 
         metadata to proceed with postprocessing.")
  }
  
  ## Check if we have all measures needed for postprocessing
  measuresNeededForPostprocessing<-.measuresNeededForPostprocessing()
  
  if("all"%in%indicators||is.null(indicators)){
    indicators<-names(measuresNeededForPostprocessing)
  }
  
  requiredMeasures<-measuresNeededForPostprocessing[indicators]%>%unlist%>%unique()
  if(any(requiredMeasures%in%outputs$measure_name==FALSE)){
    stop(paste0("To calculate chosen indicators, you need measures ",
                paste(requiredMeasures[requiredMeasures%in%outputs$measure_name==FALSE]%>%unique(),collapse=", ")," in your output files.",
                collapse=""))
  }
  outputs<-outputs[measure_name%in%requiredMeasures]
  
  ## Switch survey output to wide format and merge with metadata
  outputs<-dcast(outputs,
                 age_group+scenario_file_index+survey_date~measure_name ,
                 value.var="value")
  outputs<-merge(outputs,metadata,by="scenario_file_index")
  
  ## Start calculations
  message(paste0("Calculating epidemiological indicators: ",paste(indicators,collapse=", ")))
  
  if ("incidence" %in% indicators) {
    outputs[,incidenceRate:=(nUncomp+nSevere)/nHost]
    outputs[,incidenceRatePerThousand:=incidenceRate/1000]
  }
  
  if ("prevalence" %in% indicators) {
    outputs[,prevalenceRate:=nPatent/nHost,]
    outputs[,prevalenceRatePerThousand:=prevalenceRate/1000]
  }
  
  if ("tUncomp" %in% indicators) {
    outputs[,tUncomp:=nTreatments1+nTreatments2]
  }
  
  if ("tSevere" %in% indicators) {
    outputs[,tSevere:=nTreatments3]
  } 
  
  if ("nHosp" %in% indicators) {
    outputs[,nHosp:=nHospitalDeaths+nHospitalRecovs+nHospitalSeqs]
  } 
  
  if ("edeath" %in% indicators) {
    outputs[,edeathPerHundredThousand:=(expectedDirectDeaths+expectedIndirectDeaths)/nHost/1e5]
    outputs[,edeath:=expectedDirectDeaths+expectedIndirectDeaths]
    
  } 
  
  if ("edirdeath" %in% indicators) {
    outputs[,edirdeathPerHundredThousand:=expectedDirectDeaths/nHost/1e5]
    outputs[,edirdeathPerHundredThousand:=expectedDirectDeaths]
    
  } 
  
  if ("ddeath" %in% indicators) {
    outputs[,ddeathPerHundredThousand:=(nIndDeaths+nDirDeaths)/nHost/1e5]
    outputs[,ddeathPerHundredThousand:=nIndDeaths+nDirDeaths]
    
  } 
  
  if(is.null(keepMeasuresForIndicators)){
    keepMeasures<-NULL}else{
    keepMeasures<-measuresNeededForPostprocessing[keepMeasuresForIndicators]%>%unlist%>%unique()
    }
  removeMeasures<-setdiff(requiredMeasures,keepMeasures)
  
  outputs<-outputs[,-..removeMeasures]
  outputs[,survey_date:=as.Date(survey_date)]
  outputs[,age_group:=as.factor(age_group)]
  outputs[,scenario_file_index:=as.factor(scenario_file_index)]
  outputs[,setting:=as.factor(setting)]
  outputs[,seed:=as.factor(seed)]
  
  return(outputs)
}




