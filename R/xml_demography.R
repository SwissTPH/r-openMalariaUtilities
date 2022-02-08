### Little helpers for the the demography section

##' @title Generate list for 'demography/ageGroup'
##' @param lowerbound Double, lower bound of age group
##' @param ageGroups Data frame containing demography
##' @return List for xml contruction
##' @export
ageGroupsGen <- function(lowerbound, ageGroups) {
  ## Input validation
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(ageGroups, add = assertCol)
  checkmate::reportAssertions(collection = assertCol)
  ## Assign lowerbound
  outlist <- list(lowerbound = lowerbound)
  ## Apply expected data types
  ageGroups[c("poppercent", "upperbound")] <- vapply(
    ageGroups[c("poppercent", "upperbound")], as.double,
    FUN.VALUE = double(1), USE.NAMES = FALSE)
  outlist <- .xmlAddChunks(
    outlist = outlist, element = "group", attributeList = ageGroups
  )
  return(outlist)
}


##' @title Define and write demography input to baseXMLfile
##' @param baseXMLfile List with experiment data.
##' @param demographyData, list with demography data in specific format
##' @param popSize, number of agents in openMalaria
##' @param additional parameters used to build demographyData list if demographyData is null: upperbound, poppercent, lowerbound, name, maximumAgeYrs,source
##' @return List for xml construction
##' @export

defineDemography <- function(baseXMLfile,demographyData=NULL,popSize=3000,
                             upperbound=NULL,poppercent=NULL,lowerbound=NULL,name=NULL,maximumAgeYrs=NULL,source=NULL,) {
  ## Input validation
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertList(demographyData, null.ok=TRUE, add = assertCol)
  checkmate::assertVector(upperbound, null.ok=TRUE, add = assertCol)
  checkmate::assertVector(poppercent, null.ok=TRUE, add = assertCol)
  checkmate::assertNumeric(lowerbound, null.ok=TRUE, add = assertCol)
  checkmate::assertNumeric(maximumAgeYrs, null.ok=TRUE, add = assertCol)
  checkmate::reportAssertions(collection = assertCol)
  
  ## Build demographyData from function arguments
  if(is.null(demographyData)){
    demographyData<-list(ageGroups=data.frame(upperbound=upperbound,poppercent=poppercent),lowerbound=lowerbound,name=name,maximumAgeYrs=maximumAgeYrs,source=source)
    print(paste0("Created demographyData from your input: ",paste0(demographyData,collapse="; ")))
  }
  
  
  ## Assign name, maximumAgeYrs, lowerbound, popSize
  baseXMLfile <- .xmlAddList(
    data = baseXMLfile, sublist = c("demography"),append=F,
    entry = "name",
    input = demographyData$name
  )
  baseXMLfile <- .xmlAddList(
    data = baseXMLfile, sublist = c("demography"),append=F,
    entry = "maximumAgeYrs",
    input = demographyData$maximumAgeYrs
  )
  baseXMLfile <- .xmlAddList(
    data = baseXMLfile, sublist = c("demography"),append=F,
    entry = "popSize",
    input = popSize
  )
  
  ageGroupList<-list()
  for (idx in 1:nrow(demographyData$ageGroups)) {
    ageGroupList <- append(
      ageGroupList, list(group = list(poppercent=demographyData$ageGroups[idx,"poppercent"], upperbound=demographyData$ageGroups[idx,"upperbound"])
    ))
  }
  
  baseXMLfile <- .xmlAddList(
    data = baseXMLfile, sublist = c("demography"),append=F,
    entry = "ageGroup",
    input = ageGroupList
  )
  
  baseXMLfile <- .xmlAddList(
    data = baseXMLfile, sublist = c("demography","ageGroup"),append=T,
    entry = "lowerbound",
    input = demographyData$lowerbound
  )
  
  
  
  return(baseXMLfile)
}

