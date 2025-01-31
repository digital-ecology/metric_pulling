#' exsum baseline string
#'
#' generates the summary of the unique baseline habitats on site, to be printed in the executive summary
#'
#' @param habitattype the detailed habitat type, e.g. 'Other neutral grassland', contained in the list
#' produced by the function 'pulloffsitebaseline'.
#'
#' @return the strings of the unique habitat types present on site
#' @export
#'
#' @examples \dontrun{baselinestring<-exsum_baselinestring(habitattype)}
#' 
exsum_baselinestring<-function(habitattype){

#get unique baseline habs at site, and number of unique baseline
uniquebaseline<-unique(habitattype)
uniquebaseline<-as.data.frame(uniquebaseline)
numbaseline<-nrow(uniquebaseline)

#create string of baseline habitats
if (numbaseline > 1) {
  if (numbaseline > 2) {
    uniquebaseline_string <- paste(paste(head(uniquebaseline$uniquebaseline, -2), collapse = ", "),
                                    paste(tail(uniquebaseline$uniquebaseline, 2), collapse = " and "),
                                    sep = ", ")
  } else {
    uniquebaseline_string <- paste(uniquebaseline$uniquebaseline[1], uniquebaseline$uniquebaseline[2], sep = " and ")
  }
} else {
  uniquebaseline_string <- uniquebaseline$uniquebaseline
}

if (uniquebaseline_string == "No Existing Watercourses" |
    uniquebaseline_string == "No Existing Hedgerows"){
  uniquebaseline_string <- NULL
}

uniquebaseline_string <- ifelse(is.null(uniquebaseline_string), "nothing", uniquebaseline_string)

return(tolower(uniquebaseline_string))

}

#' exsum creation string
#'
#' generates the summary of the unique habitats to be created on site, to be printed in the executive summary
#'
#' @param createdhabitattype the detailed habitat type, e.g. 'Other neutral grassland', contained in the list
#' produced by the function 'pulloffsitecreation'.
#'
#' @return the strings of the unique habitat types to be created, returning 'nothing' if none
#' @export
#'
#' @examples \dontrun{creationstring<-exsum_creationstring(creationdata$createdhabitattype)}
exsum_creationstring<-function(createdhabitattype){

  #unique to be created
  uniquecreation <- unique(createdhabitattype)
  uniquecreation<-as.data.frame(uniquecreation)
  numcreation <- nrow(uniquecreation)

  #creation string with commas or ands where necessary
  if (numcreation > 1) {
    if (numcreation > 2) {
      uniquecreation_string <- paste(paste(head(uniquecreation$uniquecreation, -2), collapse = ", "),
                                    paste(tail(uniquecreation$uniquecreation, 2), collapse = " and "),
                                    sep = ", ")
    } else {
      uniquecreation_string <- paste(uniquecreation$uniquecreation[1], uniquecreation$uniquecreation[2], sep = " and ")
    }
  } else {
    uniquecreation_string <- uniquecreation$uniquecreation
  }

  #due to prev change, may insert No Habs Created, but to format string properly, assign this as null
  if (uniquecreation_string == "No Habitats Created" |
      uniquecreation_string == "No Hedges Created" |
      uniquecreation_string == "No Watercourses Created"){
    uniquecreation_string <- NULL
  }

  uniquecreation_string <- ifelse(is.null(uniquecreation_string), "nothing", uniquecreation_string)
  
  return(tolower(uniquecreation_string))
}

#' exsum enhancement string
#'
#' @param enhancedhabitattype the detailed habitat type, e.g. 'Other neutral grassland', contained in the list
#' produced by the function 'pulloffsiteenhancement'.
#'
#' @return the strings of the unique habitat types to be enhanced, returning 'nothing' if none
#' @export
#'
#' @examples \dontrun{enhancestring<- exsum_enhancestring(enhancedata$enhancedhabitattype)}
exsum_enhancestring<-function(enhancedhabitattype){

#enhance list
uniqueenhance <- unique(enhancedhabitattype)
uniqueenhance <-as.data.frame(uniqueenhance)
numenhance <- nrow(uniqueenhance)

if (numenhance > 1) {
  if (numenhance > 2) {
    uniqueenhance_string <- paste(paste(head(uniqueenhance$uniqueenhance, -2), collapse = ", "),
                                   paste(tail(uniqueenhance$uniqueenhance, 2), collapse = " and "),
                                   sep = ", ")
  } else {
    uniqueenhance_string <- paste(uniqueenhance$uniqueenhance[1], uniqueenhance$uniqueenhance[2], sep = " and ")
  }
} else {
  uniqueenhance_string <- uniqueenhance$uniqueenhance
}

#due to prev change, may insert No Habs Created, but to format string properly, assign this as null

if (uniqueenhance_string == "No Habitats Enhanced" |
    uniqueenhance_string == "No Hedges Enhanced" |
    uniqueenhance_string == "No Watercourses Enhanced"){
  
  uniqueenhance_string <- NULL

  }

uniqueenhance_string <- ifelse(is.null(uniqueenhance_string), "nothing", uniqueenhance_string)

return(tolower(uniqueenhance_string))

}

#' passstring
#'
#' @param netdata data showing if all linear and area based units have reached net gain, and if trading summaries are met 
#'
#' @return a string reporting if the proposals passed, and if not, what theyre mmissing 
#' @export
#'
#' @examples \dontrun{passstring<-exsum_passstring(netdata)}
exsum_passstring<-function(netdata){
  
  #sanitise numbers
  netdata$NetHabitatPercent <- trimws(netdata$NetHabitatPercent)
  netdata$NetHedgerowPercent <- trimws(netdata$NetHedgerowPercent)
  netdata$NetWaterPercent <- trimws(netdata$NetWaterPercent)
  
  passstring<-""
  
  if (netdata$NetHabitatPercent > 10 && netdata$NetHedgerowPercent > 10 && netdata$NetWaterPercent > 10 && netdata$TradeSatisfied == "Yes"){
    
    passstring <- "secures a >10% gain in biodiversity for area and linear biodiversity units, and meets the trading rules within the metric. Further details regarding habitat management and establishment will be provided as part of a later application within a Habitat Management and Monitoring Plan (HMMP)."
    
  } else if (netdata$NetHabitatPercent > 10 && netdata$NetHedgerowPercent > 10 && netdata$NetWaterPercent > 10 && netdata$TradeSatisfied != "Yes") {
    
    #in here, insert function to pull whichever type of habitat has the deficit for trading rules, and which habitat if necessary
    
    passstring<- "secures a >10% gain in biodiversity for area and linear biodiversity units, but does not meet the trading rules within the metric. To satisfy trading rules, a further X ABU and X LBU are necessary, of this distinctiveness."
    
  } else if ((netdata$NetHabitatPercent <= 10 || netdata$NetHedgerowPercent <= 10 || netdata$NetWaterPercent <= 10) && netdata$TradeSatisfied == "Yes"){
    
    #in here, report unit deficit. 
  
    passstring<- "meets the trading rules within the metric, but does not secure a >10% gain in biodiversity for all area and linear biodiversity units. A further X units are necessary to compensate."
  
  } else if ((netdata$NetHabitatPercent <= 10 || netdata$NetHedgerowPercent <= 10 || netdata$NetWaterPercent <= 10) && netdata$TradeSatisfied != "Yes"){
    
    #in here, both report unit deficit and which habitat distinctiveness must be met 
    
    passstring<- "neither meets the trading rules within the metric, nor secures a >10% gain in biodiversity for all area and linear biodiversity units. A further X units are necessary to compensate, of this distinctiveness."
    
  }
  
  return(passstring)
  
}

#' exsum loss string
#'
#' @param losthabitattype the detailed habitat type, e.g. 'Other neutral grassland', contained in the list
#' produced by the function 'pulloffsitehabitatbaseline'.
#'
#' @return the strings of the unique habitat types to be lost, returning 'nothing' if none
#' @export
#'
#' @examples \dontrun{loststring<- exsum_loststring(baselinedata$losthabitattype)}
exsum_loststring<-function(losthabitattype){
  
  #lost list
  uniquelost <- unique(losthabitattype)
  uniquelost <- as.data.frame(uniquelost)
  numlost <- nrow(uniquelost)
  
  if (numlost > 1) {
    if (numlost > 2) {
      uniquelost_string <- paste(paste(head(uniquelost$uniquelost, -2), collapse = ", "),
                                    paste(tail(uniquelost$uniquelost, 2), collapse = " and "),
                                    sep = ", ")
    } else {
      uniquelost_string <- paste(uniquelost$uniquelost[1], uniquelost$uniquelost[2], sep = " and ")
    }
  } else {
    uniquelost_string <- uniquelost$uniquelost
  }
  
  #due to prev change, may insert No Habs Created, but to format string properly, assign this as null
  
  if (uniquelost_string == "No Habitats Lost" |
      uniquelost_string == "No Hedges Lost" |
      uniquelost_string == "No Watercourses Lost"){
    
    uniquelost_string <- NULL
    
  }
  
  uniquelost_string <- ifelse(is.null(uniquelost_string), "nothing", uniquelost_string)
  
  return(tolower(uniquelost_string))
  
}
