##' @title Creates VectorInterventionParameters list from AnophelesModel package
##' used to define vector control interventions with defineVectorControl function
##' also creates MosquitoParameters list from AnophelesModel package
##' used to define mosquito bionomics with defineEntomology function
##' @param vectorSpecies vector species to be considered
##' @param interventionList list of interventions to be considered
##' @example vectorSpecies=c("Anopheles gambiae","Anopheles funestus")
##' @example interventionList=list(LLIN=list(id="LLIN",
##' description="test LLIN",
##' parameterisation="LLINs01",
##' LLIN_type="Default",
##' LLIN_insecticide="Default",
##' LLIN_country="Kenya"),
##' IRS=list(id="IRS",
##'          description="IRS",
##'          parameterisation="IRS02")
##' )
##' @export

fromAnophelesModel<-function(vectorSpecies,interventionList){

# 
# vectorSpecies=c("Anopheles gambiae","Anopheles funestus")
# interventionList=list(LLIN=list(id="LLINs",
#                       description="test LLIN",
#                       parameterisation="LLINs01",
#                       LLIN_type="Default",
#                       LLIN_insecticide="Default",
#                       LLIN_country="Kenya"),
#                       
#                       IRS=list(id="IRS",
#                                description="IRS",
#                                parameterisation="IRS02")
# )
#   

  
print(paste0("You haven chosen vector species: ",paste0(vectorSpecies,collapse=", "))) 
  
##part 1: set up entomology parameterization mosq
mosq<-sapply(vectorSpecies,
             function(x){
               ent_params = def_vector_params(mosquito_species = x) #vector bionomics with defaults for Anopheles gambiae
               host_params = def_host_params()#probabilities of state in gonotrophic cycle for human, non-human hosts with defaults from Kenya for gambiae
               return(list(ent_params=ent_params,host_params=host_params))
             },simplify=F,USE.NAMES=T)

MosquitoParameters<-sapply(mosq,
                  function(x){
                    entomology_xml = get_OM_ento_snippet(x$ent_params, x$host_params)
                    rapply(XML::xmlToList(entomology_xml$mosq_snippet),as.list,how="list")
                  },simplify=F,USE.NAMES=T)#nested list for input in openMalariaUtilities for different vector species

## part 2: generate xml snippets for context-dependent vector control interventions
activity_p = def_activity_patterns(activity = "default_Anopheles_gambiae")#set up human and mosquito activity patterns over a single day, indoor/outdoor biting rhythm, if available choose context, not all parameters come from single location, we choose default
#activity_patterns 16h00 7h00
host_pop=2000
interpolation_points<-100
model_params <-lapply(mosq, function(x){
  build_model_obj(x$ent_params, x$host_params, activity_p, host_pop)
})#initialize the entomological model and build the model object


print(paste0("Your chosen interventions are: "))
print(AnophelesModel::interventions_param$interventions_summary[AnophelesModel::interventions_param$interventions_summary$Parameterisation%in%unlist(lapply(interventionList, function(x) x$parameterisation)),])

VectorInterventions<-list()

for (interventionName in names(interventionList))
{
  interventionParameterization<-lapply(vectorSpecies,
                                       function(x) { def_interventions_effects(intervention_list=list(interventionList[[interventionName]]),
                                                                                                     model_p=model_params[[x]],
                                                                                                     num_ip_points=interpolation_points
  )[[1]]}
  )
  names(interventionParameterization)<-vectorSpecies
  VectorInterventions<-append(VectorInterventions,setNames(list(interventionParameterization),interventionName))
}

##warning, some values beyond boundary knots for b-splines for  num_ip_points=10


##calculate intervention effect size directly with Anopheles package
vec_pop<-5*host_pop
effects_dict<-c(deterrency_snippet="deterrency",preprandial_snippet="preprandialKillingEffect",postprandial_snippet="postprandialKillingEffect")
effects_list<-setNames(vector(mode="list",length=3),unlist(effects_dict))
VectorInterventionParameters<-setNames(rep(list(effects_list),length(VectorInterventions)),names(VectorInterventions))

for (it in names(VectorInterventions)){
  for (effect in names(effects_dict)){
    anophelesParams<-list()
    for (vec in vectorSpecies){
      
      impact<-calculate_impact(list(VectorInterventions[[it]][[vec]]), 
                               coverages, 
                               model_params[[vec]],
                               vec_pop,
                               interpolation_points)
      impact_snippet<-get_OM_GVI_snippet(vec,impact$interventions_vec[[1]],plot=F)
      impact_snippet<-lapply(impact_snippet, function(y) rapply(XML::xmlToList(y$GVI_xml_snippet),as.list,how="list"))
      anophelesParams<-append(anophelesParams,setNames(list(list(propActive=impact_snippet[[effect]][["anophelesParams"]][["propActive"]],
                                                                 value= impact_snippet[[effect]][[effects_dict[[effect]]]][["value"]])),
                                                       vec))
    }
    
    VectorInterventionParameters[[it]][[effects_dict[[effect]]]]<-list(decay=impact_snippet[[effect]]$decay,
                                                                       anophelesParams=anophelesParams)
  }
}


output<-list(VectorInterventionParameters=VectorInterventionParameters,MosquitoParameters=MosquitoParameters)

return(output)
}
