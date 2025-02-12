#' pull onsite baseline data for summary
#'
#' @param metric feas metric
#'
#' @return a list, containing dataframes pulled from the baseline sheet of the metric
#'
#' @export
#'
#' @examples \dontrun{baselinedata<-pullonsitehabitatbaseline(metric)}
pullonsitehabitatbaseline<-function(metric){
    #baselinesheet
    baselinesheet <- "A-1 On-Site Habitat Baseline"
    
    #Broad Habitat
    broadbaseline<- openxlsx::read.xlsx(metric, baselinesheet, cols = 5, colNames = FALSE, startRow = 11)
    colnames(broadbaseline) <- "broadhabitat"
    
    #Specific Habitat
    baselinehabitattype<- openxlsx::read.xlsx(metric, baselinesheet, cols = 6, colNames = FALSE, startRow = 11)
    #remove last three rows from habtiattype as they are rows abt area/hectares etc
    baselinehabitattype <- baselinehabitattype[1:(nrow(baselinehabitattype) - 3), , drop = FALSE]
    colnames(baselinehabitattype) <- "habitattype"
    
    #Baseline Area
    baselinearea<-openxlsx::read.xlsx(metric, sheet = baselinesheet, cols = 8, colNames = FALSE, startRow = 11)
    baselinearea <- baselinearea[1:(nrow(baselinearea) - 3), , drop = FALSE] #remove last cols with writing in
    baselinearea$X1<-as.numeric(baselinearea$X1)
    colnames(baselinearea) <- "baselinearea"
    
    #baselinedistinctiveness
    baselinedistinctiveness<-openxlsx::read.xlsx(metric, sheet = baselinesheet, cols = 9, colNames = FALSE, startRow = 11)
    baselinedistinctiveness<- baselinedistinctiveness[baselinedistinctiveness$X1!= "", , drop = FALSE]
    baselinedistinctiveness<- baselinedistinctiveness[-nrow(baselinedistinctiveness), , drop = FALSE]
    baselinedistinctiveness$X1 <- ifelse(baselinedistinctiveness$X1 == "V.low", "Very Low", baselinedistinctiveness$X1)
    baselinedistinctiveness$X1 <- ifelse(baselinedistinctiveness$X1 == "V.Low", "Very Low", baselinedistinctiveness$X1)
    baselinedistinctiveness$X1 <- ifelse(baselinedistinctiveness$X1 == "V.high", "Very High", baselinedistinctiveness$X1)
    colnames(baselinedistinctiveness) <- "distinctiveness"
    
    #BaselineCondition
    baselinecondition <-openxlsx::read.xlsx(metric, sheet = baselinesheet, cols = 11, colNames = FALSE, startRow = 11)
    colnames(baselinecondition) <- "baselinecondition"
    
    #BaselineStrategicSignificance
    baseliness <-openxlsx::read.xlsx(metric, sheet = baselinesheet, cols = 13, colNames = FALSE, startRow = 11)
    #this pulls all 200, think cus yellow cells, so maybe do based on prev col and assign low/med/high?
    baseliness$X1 <- ifelse(baseliness$X1 == "Area/compensation not in local strategy/ no local strategy", "Low", baseliness$X1)
    baseliness$X1 <- ifelse(baseliness$X1 == "Location ecologically desirable but not in local strategy", "Medium", baseliness$X1)
    baseliness$X1 <- ifelse(baseliness$X1 == "Formally identified in local strategy", "High", baseliness$X1)
    colnames(baseliness) <- "baseliness"
    
    #BaselineBNGUnits
    baselinebngu<-openxlsx::read.xlsx(metric, sheet = baselinesheet, cols = 17, colNames = FALSE, startRow = 11)
    baselinebngu <- baselinebngu[baselinebngu$X1 != "", , drop = FALSE]
    baselinebngu <- baselinebngu[-nrow(baselinebngu), , drop = FALSE] #remove final line as total
    baselinebngu$X1<-as.numeric(baselinebngu$X1)
    colnames(baselinebngu) <- "baselineabu"
    
    habitatbaselinedata<-data.frame(broadbaseline, baselinehabitattype, baselinearea, baselinedistinctiveness, baselinecondition, baseliness, baselinebngu)
    
    #get total areas, and total baseline units
    totalarea<-openxlsx::read.xlsx(metric, sheet = baselinesheet, cols = 8, rows = 259, colNames = FALSE)
    colnames(totalarea) <- "totalarea"
    totalunits<-openxlsx::read.xlsx(metric, sheet = baselinesheet, cols = 17, rows = 259, colNames = FALSE)
    colnames(totalunits) <- "totalbaselineabu"
    
    habitatbaselinedata<-list(habitatbaselinedata = habitatbaselinedata,
                              totalarea = totalarea,
                              totalunits = totalunits)
  
    return(habitatbaselinedata)

}

#' pullhabitatretain
#'
#' @param metric metric
#'
#' @return retain df
#' @export
#'
#' @examples \dontrun{retaindata<-pullonsitehabitatretain(metric)}
pullonsitehabitatretain<-function(metric){

  
    #baselinesheet
    baselinesheet <- "A-1 On-Site Habitat Baseline"
    
    #Broad Habitat
    broadbaseline<- openxlsx::read.xlsx(metric, baselinesheet, cols = 5, colNames = FALSE, startRow = 11)
    colnames(broadbaseline) <- "broadhabitat"
    
    #Specific Habitat
    baselinehabitattype<- openxlsx::read.xlsx(metric, baselinesheet, cols = 6, colNames = FALSE, startRow = 11)
    #remove last three rows from habtiattype as they are rows abt area/hectares etc, turn back into df, assign colname
    baselinehabitattype <- baselinehabitattype[1:(nrow(baselinehabitattype) - 3), , drop = FALSE]
    colnames(baselinehabitattype) <- "habitattype"
    
    #area retained
    retainareas<-openxlsx::read.xlsx(metric, sheet = baselinesheet, cols = 19, startRow = 10, colNames = TRUE, skipEmptyRows = FALSE)
    retainareas <- retainareas[1:nrow(baselinehabitattype), , drop = FALSE]
    retainareas[is.na(retainareas)] <- 0
    retainareas$Area.retained<-as.numeric(retainareas$Area.retained)
    colnames(retainareas) <- "arearetained"
    
    #units retained
    retainunits<-openxlsx::read.xlsx(metric, sheet = baselinesheet, cols = 21, startRow = 10, colNames = TRUE, skipEmptyRows = FALSE)
    retainunits <- retainunits[1:nrow(baselinehabitattype), , drop = FALSE]
    colnames(retainunits) <- "aburetained"
    
    retainedhabitatdata<-data.frame(broadbaseline, baselinehabitattype, retainareas, retainunits)
    retainedhabitatdata <- retainedhabitatdata[!(retainedhabitatdata$arearetained == 0 & retainedhabitatdata$aburetained == 0), ]
    
    if (nrow(retainedhabitatdata) == 0) {retainedhabitatdata<-data.frame(broadhabitat = "No Habitats Retained")}
    
    #and get total retain area
    totalretainarea<-openxlsx::read.xlsx(metric, sheet = baselinesheet, cols = 19, rows = 259, colNames = FALSE)
    colnames(totalretainarea) <- "totalarearetained"
    
    #get total retain units
    totalretainunits<-openxlsx::read.xlsx(metric, sheet = baselinesheet, cols = 21, rows = 259, colNames = FALSE)
    colnames(totalretainunits) <- "totalaburetained"
    
    retainhabitatdata<-list(habitatretaindata = retainedhabitatdata,
                            TotalRetainArea = totalretainarea,
                            TotalRetainUnits = totalretainunits)
    
    return(retainhabitatdata)
  
}

#' pullhabitatloss
#'
#' @param metric metric
#'
#' @return loss df
#' @export
#'
#' @examples \dontrun{habitatlossdata<-pullonsitehabitatloss(metric)}
pullonsitehabitatloss<-function(metric){
   #baselinesheet
    baselinesheet <- "A-1 On-Site Habitat Baseline"
    
    #Broad Habitat
    broadbaseline<- openxlsx::read.xlsx(metric, baselinesheet, cols = 5, colNames = FALSE, startRow = 11)
    colnames(broadbaseline) <- "broadhabitat"
    
    #Specific Habitat
    baselinehabitattype<- openxlsx::read.xlsx(metric, baselinesheet, cols = 6, colNames = FALSE, startRow = 11)
    #remove last three rows from habtiattype as they are rows abt area/hectares etc, turn back into df, assign colname
    baselinehabitattype <- baselinehabitattype[1:(nrow(baselinehabitattype) - 3), , drop = FALSE]
    colnames(baselinehabitattype) <- "habitattype"
    
    #area lost
    lostareas<-openxlsx::read.xlsx(metric, sheet = baselinesheet, cols = 23, startRow = 10, colNames = TRUE, skipEmptyRows = FALSE)
    lostareas<-lostareas[1:nrow(baselinehabitattype), , drop = FALSE]
    lostareas$Area.habitat.lost<-as.numeric(lostareas$Area.habitat.lost)
    colnames(lostareas) <- "arealost"
    
    #units lost
    lostunits<-openxlsx::read.xlsx(metric, sheet = baselinesheet, cols = 24, startRow = 10, colNames = TRUE, skipEmptyRows = FALSE)
    lostunits<-lostunits[1:nrow(baselinehabitattype), , drop = FALSE]
    colnames(lostunits) <- "abulost"
    
    losthabitatdata<-data.frame(broadbaseline, baselinehabitattype, lostareas, lostunits)
    #this just pulls all, so filter out areas where both are 0 as that means no area / units lost
    losthabitatdata <- losthabitatdata[!(losthabitatdata$arealost == 0 & losthabitatdata$abulost == 0), ]
    
    #and get total lost area
    totallostarea<-openxlsx::read.xlsx(metric, sheet = baselinesheet, cols = 23, rows = 259, colNames = FALSE)
    colnames(totallostarea) <- "totalarealost"
    
    #get total lost units
    totallostunits<-openxlsx::read.xlsx(metric, sheet = baselinesheet, cols = 24, rows = 259, colNames = FALSE)
    colnames(totallostunits) <- "totalabulost"
    
    lostdata<-list(habitatlostdata = losthabitatdata,
                   TotallostArea = totallostarea,
                   TotallostUnits = totallostunits)
    
    return(lostdata)
}

#' pull onsite habitats to be created
#'
#' @param metric feas metric
#'
#' @return a list, containing data about habitats to be created
#' @export
#'
#' @examples \dontrun{creationdata<-pullonsitehabitatcreation(metric)}
pullonsitehabitatcreation<-function(metric){
  
    #creation bits
    creationsheet <- "A-2 On-Site Habitat Creation"
    
    #broad habitat
    broadcreation <- openxlsx::read.xlsx(metric, sheet = creationsheet, cols = 4, colNames = FALSE, startRow = 11)
    
    #having issues when nothing new is created, so need to do an if else clause
    if(is.null(broadcreation)) {
      broadcreation <- data.frame(broadhabitat =  "No Habitats Created")
    } else {
      colnames(broadcreation) <- "broadhabitat"
    }
    
    #habitat type
    creationhabitattype<- openxlsx::read.xlsx(metric, sheet = creationsheet, cols = 5, colNames = FALSE, startRow = 11)
    
    #as this will always pull three total columns, needs to be if else for greater than 3
    if (nrow(creationhabitattype) <= 3){
      
      #if no creation data, enter placeholder saying no habs created
      creationhabitattype <- data.frame(habitattype = "No Habitats Created")
      
    } else {
      
      #get rid of final three columns to do with area, turn back into df, assign colnames
      creationhabitattype <- creationhabitattype[1:(nrow(creationhabitattype) - 3), , drop = FALSE]
      colnames(creationhabitattype)<-"habitattype"
      
    }
    
    #only pull the rest of the creation data if there are habitats to pull!
    
    if (!"No Habitats Created" %in% broadcreation$broadhabitat){
      
      #area
      creationarea<-openxlsx::read.xlsx(metric, sheet = creationsheet, cols = 7, startRow = 11, colNames = FALSE)
      #remove last hree rows
      creationarea<-creationarea[1:(nrow(creationarea) - 3), , drop = FALSE]
      creationarea$X1<-as.numeric(creationarea$X1)
      colnames(creationarea)<-"createdarea"
      
      #condition
      creationcond<-openxlsx::read.xlsx(metric, sheet = creationsheet, cols = 10, startRow = 11, colNames = FALSE)
      colnames(creationcond)<-"createdcondition"
      
      #SS
      creationss <-openxlsx::read.xlsx(metric, sheet=creationsheet, cols = 12, startRow = 11, colNames = FALSE)
      creationss$X1 <- ifelse(creationss$X1 == "Area/compensation not in local strategy/ no local strategy", "Low", creationss$X1)
      creationss$X1 <- ifelse(creationss$X1 == "Location ecologically desirable but not in local strategy", "Medium", creationss$X1)
      creationss$X1 <- ifelse(creationss$X1 == "Formally identified in local strategy", "High", creationss$X1)
      colnames(creationss)<-"createdss"
      
      #action
      creationaction<-data.frame(Action = rep('Create', nrow(creationss)))
      
      #distinctiveness
      creationdistinctiveness<-openxlsx::read.xlsx(metric, sheet = creationsheet, cols = 8, colNames = FALSE, startRow = 11)
      creationdistinctiveness<- creationdistinctiveness[creationdistinctiveness$X1!= "", , drop = FALSE]
      creationdistinctiveness<- creationdistinctiveness[-nrow(creationdistinctiveness), , drop = FALSE]
      creationdistinctiveness$X1 <- ifelse(creationdistinctiveness$X1 == "V.low", "Very Low", creationdistinctiveness$X1)
      creationdistinctiveness$X1 <- ifelse(creationdistinctiveness$X1 == "V.Low", "Very Low", creationdistinctiveness$X1)
      creationdistinctiveness$X1 <- ifelse(creationdistinctiveness$X1 == "V.high", "Very High", creationdistinctiveness$X1)
      colnames(creationdistinctiveness) <- "distinctiveness"
      
      
      #cant pull time as in hashed out cell
      creationtime <-openxlsx::read.xlsx(metric, sheet=creationsheet, cols = 19, startRow = 11, colNames = FALSE)
      creationtime<- creationtime[creationtime$X1 != "", , drop = FALSE]
      colnames(creationtime) <- "createdtime"
      
      #cant pull bng units as in hashed out cell
      creationunits <-openxlsx::read.xlsx(metric, sheet=creationsheet, cols = 25, startRow = 11, colNames = FALSE)
      creationunits<- creationunits[creationunits$X1 != "", , drop = FALSE]
      creationunits<-creationunits[1:(nrow(creationunits) - 1), , drop = FALSE]
      creationunits$X1<-as.numeric(creationunits$X1)
      colnames(creationunits) <- "createdabu"
      
      habitatcreationdata<-data.frame(broadcreation, creationhabitattype, creationarea,
                                      creationcond, creationss, creationdistinctiveness, creationaction,
                                      creationtime, creationunits)
      
      
      
    } else {
      
      habitatcreationdata<-data.frame(broadcreation, creationhabitattype)
    }
    
    #finally, get totals and add to whatever the creation data list is
    totalcreationarea<-openxlsx::read.xlsx(metric, sheet = creationsheet, cols = 7, rows = 257, colNames = FALSE)
    colnames(totalcreationarea)<-"totalareacreated"
    
    totalcreationunits<-openxlsx::read.xlsx(metric, sheet = creationsheet, cols = 25, rows = 257, colNames = FALSE)
    colnames(totalcreationunits)<-"totalabucreated"
    
    habitatcreationdata <- list(habitatcreationdata = habitatcreationdata,
                                TotalCreationArea = totalcreationarea,
                                TotalCreationUnits = totalcreationunits)
  
    return(habitatcreationdata)
}

#' pull onsite habitats to be enhanced
#'
#' @param metric feas metric
#'
#' @return a list, containing 3 elements: BroadEnhance, the broad habitat type, e.g 'Grassland',
#' EnhanceHabitatType, the detailed habitat type, e.g. 'Other neutral grassland', and then
#' EnhanceList, a list of strings summarising the habitat types proposed for enhancement on the site.
#' @export
#'
#' @examples \dontrun{enhancedata<-pullonsitehabitatenhancement(metric)}
pullonsitehabitatenhancement<-function(metric){
    #enhancement bits
    enhancementsheet <-"A-3 On-Site Habitat Enhancement"
    
    #pull broad enhancement habitat type
    broadenhance <- openxlsx::read.xlsx(metric, sheet = enhancementsheet, cols = 17, colNames = FALSE, startRow = 12, skipEmptyRows = TRUE)
    
    #pulls 247 as macros, so remove all rows where nothin
    broadenhance <- broadenhance[broadenhance$X1 != "", , drop = FALSE]
    
    #if this DF is empty, then just paste 'No Habitats Enhanced'
    if(nrow(broadenhance) == 0) {
      broadenhance <- data.frame(broadhabitat =  "No Habitats Enhanced")
    } else {
      colnames(broadenhance) <- "broadhabitat"
    }
    
    #pulls only the specific habitat types, which are null if nothing so need catch to assign colnames
    enhancehabitattype<-openxlsx::read.xlsx(metric, sheet = enhancementsheet, cols = 18, colNames = FALSE, startRow = 12)
    
    if (is.null(enhancehabitattype)){
      enhancehabitattype <- data.frame(habitattype = "No Habitats Enhanced")
    } else {
      colnames(enhancehabitattype) <-"habitattype"
    }
    
    #only pull rest of data if there is info!
    if (!"No Habitats Enhanced" %in% broadenhance){
      
      basehabitattype <- openxlsx::read.xlsx(metric, sheet = enhancementsheet, cols = 6, colNames = FALSE, startRow = 12, skipEmptyRows = TRUE)
      basehabitattype<-basehabitattype[basehabitattype$X1 !="", , drop = FALSE]
      basehabitattype$X1 <- sub(".*- ", "", basehabitattype$X1)
      colnames(basehabitattype) <- "basehabitattype"
      
      enhancearea<-openxlsx::read.xlsx(metric, sheet = enhancementsheet, cols = 22, startRow = 12, colNames = FALSE) #pulls 247 >:(
      enhancearea<-enhancearea[enhancearea$X1 !="", , drop = FALSE]
      enhancearea<-enhancearea[1:(nrow(enhancearea) - 1), , drop = FALSE]
      colnames(enhancearea) <- "enhancedarea"
      enhancearea$enhancedarea <-as.numeric(enhancearea$enhancedarea)
      
      #distinctiveness
      enhancedist<-openxlsx::read.xlsx(metric, sheet = enhancementsheet, cols = 23, startRow = 12, colNames = FALSE) #pulls 247 >:(
      enhancedist<-enhancedist[enhancedist$X1 !="", , drop = FALSE]
      colnames(enhancedist) <- "distinctiveness"
      
      #condition
      enhancecond<-openxlsx::read.xlsx(metric, sheet = enhancementsheet, cols = 25, startRow = 12, colNames = FALSE)
      colnames(enhancecond)<-"enhancedcondition"
      
      #SS
      enhancess <-openxlsx::read.xlsx(metric, sheet=enhancementsheet, cols = 27, startRow = 12, colNames = FALSE)
      enhancess$X1 <- ifelse(enhancess$X1 == "Area/compensation not in local strategy/ no local strategy", "Low", enhancess$X1)
      enhancess$X1 <- ifelse(enhancess$X1 == "Location ecologically desirable but not in local strategy", "Medium", enhancess$X1)
      enhancess$X1 <- ifelse(enhancess$X1 == "Formally identified in local strategy", "High", enhancess$X1)
      colnames(enhancess)<- "enhancedss"
      
      #action
      enhanceaction<-data.frame(Action = rep('Enhance', nrow(enhancess)))
      
      #cant pull time as in hashed out cell
      enhancetime <-openxlsx::read.xlsx(metric, sheet=enhancementsheet, cols = 30, startRow = 12, colNames = FALSE)
      enhancetime<- enhancetime[enhancetime$X1 != "", , drop = FALSE]
      colnames(enhancetime) <- "enhancedtime"
      
      #cant pull bng units as in hashed out cell
      enhanceunits <-openxlsx::read.xlsx(metric, sheet=enhancementsheet, cols = 40, startRow = 12, colNames = FALSE)
      enhanceunits<- enhanceunits[enhanceunits$X1 != "", , drop = FALSE]
      enhanceunits<-enhanceunits[1:(nrow(enhanceunits) - 1), , drop = FALSE]
      enhanceunits$X1<-as.numeric(enhanceunits$X1)
      colnames(enhanceunits)<-"enhancedabu"
      
      enhancedata<-data.frame(basehabitattype, broadenhance, enhancehabitattype, enhancearea,
                              enhancedist, enhancecond, enhancess, enhanceaction, 
                              enhancetime, enhanceunits)
      
    } else {
      
      enhancedata<-data.frame(broadenhance, enhancehabitattype)
    }
    
    #finally, get totals and add on to the list
    totalenhancearea<-openxlsx::read.xlsx(metric, sheet = enhancementsheet, cols = 22, rows = 258, colNames = FALSE)
    colnames(totalenhancearea)<-"totalareaenhanced"
    
    totalenhanceunits<-openxlsx::read.xlsx(metric, sheet = enhancementsheet, cols = 40, rows = 258, colNames = FALSE)
    colnames(totalenhanceunits)<-"totalabuenhanced"
    
    enhancedata <- list(habitatenhancementdata = enhancedata,
                        TotalEnhanceArea = totalenhancearea,
                        TotalEnhanceUnits = totalenhanceunits)
  
    return(enhancedata)
}