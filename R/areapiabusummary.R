#' abupilost
#'
#' @param habitatlostdata hab loss df
#' @param sectioncounter counter passed to following dynamic area based post intervention sections 
#'
#' @return sectioncounter, passed to future dynamically rendered sections under post intervention
#' @export
#'
#' @examples \dontrun{sectioncounter<-abupilost(habitatlossdata$habitatlostdata, sectioncounter}
abupilost<-function(habitatlostdata, sectioncounter){
  
  sectioncounter <- sectioncounter + 1
  
  cat(sprintf("## 7.%s Lost Area Biodiversity Units\n\n", sectioncounter))  
    
  #do in sections by broad habitat type 
  broadhabs<-unique(habitatlostdata$broadhabitat)
  
  subsectioncounter <- 0  
  
for (broadhabitat in broadhabs){
    
    #print broad hab title
    cat(sprintf("### %s\n\n", broadhabitat))
    
    filteredhabitatlostdata <- habitatlostdata[habitatlostdata$broadhabitat == broadhabitat,]
  
    for (i in 1:nrow(filteredhabitatlostdata)){
    
    subsectioncounter <- subsectioncounter + 1
    
    #render paragraph number
    cat(sprintf("7.%s.%s", sectioncounter, subsectioncounter))
    cat(paste(" A ",(round(filteredhabitatlostdata$arealost[i], 2)), "ha area of ", tolower(filteredhabitatlostdata$habitattype[i])," will be lost to the development.\n\n", sep=''))
    
  }}
  return(sectioncounter)
  }

#' abupiretain
#'
#' @param habitatretaindata hab loss df
#' @param sectioncounter counter passed to following dynamic area based post intervention sections 
#'
#' @return sectioncounter, passed to future dynamically rendered sections under post intervention
#' @export
#'
#' @examples \dontrun{sectioncounter<-abupiretain(habitatretaindata$habitatretaindata, sectioncounter)}
abupiretain<-function(habitatretaindata, sectioncounter){
  
  sectioncounter <- sectioncounter + 1
  
   cat(sprintf("## 7.%s Retained Area Biodiversity Units\n\n", sectioncounter))  
    
    #do in sections by broad habitat type 
    broadhabs<-unique(habitatretaindata$broadhabitat)
    
    subsectioncounter <- 0    
    
    for (broadhabitat in broadhabs){
      
      #print broad hab title
      cat(sprintf("### %s\n\n", broadhabitat))
      
      filteredhabitatretaindata <- habitatretaindata[habitatretaindata$broadhabitat == broadhabitat,]

  for (i in 1:nrow(filteredhabitatretaindata)){
    
    subsectioncounter <- subsectioncounter + 1
    
    #render paragraph number
    cat(sprintf("7.%s.%s", sectioncounter, subsectioncounter))
    cat(paste(" A ",(round(filteredhabitatretaindata$arearetained[i], 2)), "ha area of ", tolower(filteredhabitatretaindata$habitattype[i])," will be retained.\n\n", sep=''))
    
  }}
     return(sectioncounter)
    }

#' abupicreate
#'
#' @param habitatcreationdata hab loss df
#' @param sectioncounter counter passed to following dynamic area based post intervention sections 
#'
#' @return sectioncounter, passed to future dynamically rendered sections under post intervention
#' @export
#'
#' @examples \dontrun{sectioncounter<-abupicreate(habitatcreationdata$habitatcreationdata, sectioncounter)}
abupicreate<-function(habitatcreationdata, sectioncounter){
  
  sectioncounter <- sectioncounter + 1
  
    cat(sprintf("## 7.%s Created Area Biodiversity Units\n\n", sectioncounter))  
    
    #do in sections by broad habitat type 
    broadhabs<-unique(habitatcreationdata$broadhabitat)
    
    subsectioncounter <- 0    
    
    for (broadhabitat in broadhabs){
      
      #print broad hab title
      cat(sprintf("### %s\n\n", broadhabitat))
      
      filteredhabitatcreationdata <- habitatcreationdata[habitatcreationdata$broadhabitat == broadhabitat,]
 
  for (i in 1:nrow(filteredhabitatcreationdata)){
    
    subsectioncounter <- subsectioncounter + 1
    
    #render paragraph number
    cat(sprintf("7.%s.%s", sectioncounter, subsectioncounter))
    cat(paste(" A ",(round(filteredhabitatcreationdata$createdarea[i], 2)), "ha area of ", tolower(filteredhabitatcreationdata$habitattype[i])," will be created, ", sep=''))
    if (filteredhabitatcreationdata$createdcondition[i] %in% c("Condition Assessment N/A", "N/A - Other")) {
      
      cat(paste("a ",tolower(filteredhabitatcreationdata$distinctiveness[i])," distinctiveness, ",tolower(filteredhabitatcreationdata$createdss[i])," strategic significance habitat. ", sep=''))
      
    } else {
      
      cat(paste("a ",tolower(filteredhabitatcreationdata$distinctiveness[i])," distinctiveness, ",tolower(filteredhabitatcreationdata$createdss[i])," strategic significance habitat. ", sep=''))
      cat(paste("The target condition for this proposed habitat parcel is '",tolower(filteredhabitatcreationdata$createdcondition[i]),"'. ", sep=''))
    
    }
    
    cat(paste("To facilitate the creation of this habitat...\n\n"))
    
  }}
     return(sectioncounter)
    }

#' abupienhance
#'
#' @param habitatenhancementdata hab en df
#' @param sectioncounter counter passed to following dynamic area based post intervention sections 
#'
#' @return sectioncounter, passed to future dynamically rendered sections under post intervention
#' @export
#'
#' @examples \dontrun{sectioncounter<- abupienhance(habitatenhancedata$habitatenhancementdata, sectioncounter)}
abupienhance<-function(habitatenhancementdata, sectioncounter){
  
  sectioncounter <- sectioncounter + 1
  
  cat(sprintf("## 7.%s Enhanced Area Biodiversity Units\n\n", sectioncounter))  
    
    #do in sections by broad habitat type 
    broadhabs<-unique(habitatenhancementdata$broadhabitat)
    
    subsectioncounter <- 0
    
    for (broadhabitat in broadhabs){
      
      #print broad hab title
      cat(sprintf("### %s\n\n", broadhabitat))
      
      filteredhabitatenhancementdata <- habitatenhancementdata[habitatenhancementdata$broadhabitat == broadhabitat,]
  
  for (i in 1:nrow(filteredhabitatenhancementdata)){
    
    subsectioncounter <- subsectioncounter + 1
    
    #render paragraph number
    cat(sprintf("7.%s.%s", sectioncounter, subsectioncounter))
    cat(paste(" A ",(round(filteredhabitatenhancementdata$enhancedarea[i], 2)), "ha area of ", tolower(filteredhabitatenhancementdata$broadhabitat[i])," will be enhanced from ",tolower(filteredhabitatenhancementdata$basehabitattype[i])," to ",tolower(filteredhabitatenhancementdata$habitattype[i]),", ", sep=''))
    cat(paste("a ",tolower(filteredhabitatenhancementdata$distinctiveness[i])," distinctiveness, ",tolower(filteredhabitatenhancementdata$enhancedss[i])," strategic significance habitat. ", sep=''))
    cat(paste("The target condition for this habitat enhancement is '",tolower(filteredhabitatenhancementdata$enhancedcondition[i]),"'. ", sep=''))
    cat(paste("To facilitate such an enhancement, the habitat should be...\n\n"))
    
  }}
    
      return(sectioncounter)
      
    }