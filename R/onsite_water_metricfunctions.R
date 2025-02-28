#' pulls on site water baseline data
#'
#' @param metric metric
#'
#' @return waterbaselinedata
#' @export
#'
#' @examples \dontrun{waterbaselinedata<-pullonsitewaterbaseline(metric)}
pullonsitewaterbaseline<-function(metric){
  
    #baselinesheet
    baselinesheet <- "C-1 On-Site WaterC' Baseline"
    
    #Broad Habitat
    waterhabitattype<- openxlsx::read.xlsx(metric, baselinesheet, cols = 4, colNames = FALSE, startRow = 10)
    
    #if nothing on baseline, insert placeholder so code doesnt crash
    if(is.null(waterhabitattype)) {
      waterhabitattype <- data.frame(habitattype = "No Existing Watercourses") 
    } else {
      colnames(waterhabitattype) <- "habitattype"
    }
    
    #now run rest as long as there is data
    if (!"No Existing Watercourses" %in% waterhabitattype){
      
      #Length
      waterlength<- openxlsx::read.xlsx(metric, baselinesheet, cols = 5, colNames = FALSE, startRow = 10)
      waterlength<-waterlength[1:(nrow(waterlength) -1),, drop = FALSE]
      colnames(waterlength) <- "baselinelength"
      
      #Condition
      watercondition <- openxlsx::read.xlsx(metric, baselinesheet, cols = 8, colNames = FALSE, startRow = 10)
      colnames(watercondition) <- "baselinecondition"
      
      #StrategicSignificance
      waterss <-openxlsx::read.xlsx(metric, baselinesheet, cols = 10, colNames = FALSE, startRow = 10) 
      waterss$X1 <- ifelse(waterss$X1 == "Area/compensation not in local strategy/ no local strategy", "Low", waterss$X1)
      waterss$X1 <- ifelse(waterss$X1 == "Location ecologically desirable but not in local strategy", "Medium", waterss$X1)
      waterss$X1 <- ifelse(waterss$X1 == "Formally identified in local strategy", "High", waterss$X1)
      colnames(waterss) <- "baseliness"
      
      #BaselineUnits
      waterunits<- openxlsx::read.xlsx(metric, baselinesheet, cols = 23, colNames = FALSE, startRow = 10) 
      waterunits <- waterunits[waterunits$X1 != "", , drop = FALSE]
      waterunits <- waterunits[-nrow(waterunits), , drop = FALSE]
      waterunits$X1 <- as.numeric(waterunits$X1)
      colnames(waterunits) <- "baselinelbu"
      
      #distinctiveness
      waterdist <- openxlsx::read.xlsx(metric, baselinesheet, cols = 6, colNames = FALSE, startRow = 10)
      waterdist <- waterdist[waterdist$X1 != "", , drop = FALSE]
      colnames(waterdist) <- "distinctiveness"
      
      
      waterbaselinedata<-data.frame(waterhabitattype, waterlength, watercondition,
                                    waterss, waterunits, waterdist)
      
    } else {
      
      waterbaselinedata<-data.frame(waterhabitattype)
      
    }
    
    #TotalwaterLength
    totalwaterlength <- openxlsx::read.xlsx(metric, baselinesheet, cols = 5, colNames = FALSE, startRow = 258)
    totalwaterlength<-round(totalwaterlength, 2)
    colnames(totalwaterlength) <- "totallength"
    
    #TotalwaterUnits
    totalwaterunits<-openxlsx::read.xlsx(metric, baselinesheet, cols = 23, colNames = FALSE, startRow = 258)
    totalwaterunits<-round(totalwaterunits, 2)
    colnames(totalwaterunits) <- "totallbu"
    
    # #EnhanceLengths
    # waterenhancelength<-openxlsx::read.xlsx(metric, baselinesheet, cols = 14, colNames = FALSE, startRow = 10)
    # waterenhancelength <- waterenhancelength[waterenhancelength$X1 != "", , drop = FALSE]
    # waterenhancelength$X1 <- as.numeric(waterenhancelength$X1)
    # colnames(waterenhancelength) <- "WaterLengthEnhanced"
    # 
    # #EnhanceUnits
    # waterenhanceunits<-openxlsx::read.xlsx(metric, baselinesheet, cols = 24, colNames = FALSE, startRow = 10)
    # waterenhanceunits <- waterenhanceunits[waterenhanceunits$X1 != "", , drop = FALSE]
    # waterenhanceunits <- waterenhanceunits[-nrow(waterenhanceunits), , drop = FALSE]
    # waterenhanceunits$X1 <- as.numeric(waterenhanceunits$X1)
    # colnames(waterenhanceunits) <- "WaterUnitsEnhanced"
    
    waterbaselinedata<-list(waterbaselinedata = waterbaselinedata,
                            #LengthEnhanced = waterenhancelength,
                            #UnitsEnhanced = waterenhanceunits,
                            totallength = totalwaterlength,
                            totalunits = totalwaterunits)
    
    return(waterbaselinedata)
  
}

#' pull water retain
#'
#' @param metric metric 
#'
#' @return water retain data
#' @export
#'
#' @examples \dontrun{waterretaindata<-pullonsitewaterretain(metric)}
pullonsitewaterretain<-function(metric){
  
    #baselinesheet
    baselinesheet <- "C-1 On-Site WaterC' Baseline"
    
    #Broad Habitat
    waterhabitattype<- openxlsx::read.xlsx(metric, baselinesheet, cols = 4, colNames = FALSE, startRow = 10)
    
    if(is.null(waterhabitattype)) {
      waterhabitattype <- data.frame(habitattype = "No Watercourses Retained") 
    } else {
      colnames(waterhabitattype) <- "habitattype"
    }
    
    #now run rest as long as there is data
    if (!"No Watercourses Retained" %in% waterhabitattype){
      
      #RetainLengths
      waterretainlength<-openxlsx::read.xlsx(metric, baselinesheet, cols = 23, colNames = FALSE, startRow = 10)
      waterretainlength <- waterretainlength[waterretainlength$X1 != "", , drop = FALSE]
      waterretainlength <- waterretainlength[-nrow(waterretainlength), , drop = FALSE]
      waterretainlength$X1 <- as.numeric(waterretainlength$X1)
      colnames(waterretainlength) <- "lengthretained"
      
      #RetainUnits
      waterretainunits<-openxlsx::read.xlsx(metric, baselinesheet, cols = 23, colNames = FALSE, startRow = 10)
      waterretainunits <- waterretainunits[waterretainunits$X1 != "", , drop = FALSE]
      waterretainunits <- waterretainunits[-nrow(waterretainunits), , drop = FALSE]
      waterretainunits$X1 <- as.numeric(waterretainunits$X1)
      colnames(waterretainunits) <- "lburetained"
      
      waterretaindata<-data.frame(waterhabitattype, waterretainlength, waterretainunits)
      #remove rows where both are 0 cus not retained
      waterretaindata <- waterretaindata[!(waterretaindata$lengthretained == 0 & waterretaindata$lburetained == 0), ]
      
      #if youre left with nothing, insert placeholder
      if (nrow(waterretaindata) == 0) {waterretaindata <- data.frame(habitattype = "No Watercourses Retained")}
      
    } else {
      
      waterretaindata <- data.frame(waterhabitattype)
      
    }
    
    waterretaindata<-list(waterretaindata = waterretaindata)
    
    return(waterretaindata)
  
}

#' pull water loss
#'
#' @param metric metric 
#'
#' @return water loss data
#' @export
#'
#' @examples \dontrun{waterlossdata<-pullonsitewaterloss(metric)}
pullonsitewaterloss<-function(metric){
    #baselinesheet
    baselinesheet <- "C-1 On-Site WaterC' Baseline"
    
    #Broad Habitat
    waterhabitattype<- openxlsx::read.xlsx(metric, baselinesheet, cols = 4, colNames = FALSE, startRow = 10)
    
    #if nothing on baseline, insert placeholder so code doesnt crash
    if(is.null(waterhabitattype)) {
      waterhabitattype <- data.frame(habitattype = "No Watercourses Lost") 
    } else {
      colnames(waterhabitattype) <- "habitattype"
    }
    
    #now run rest as long as there is data
    if (!"No Watercourses Lost" %in% waterhabitattype){
      
      #LostLength
      waterlostlength<-openxlsx::read.xlsx(metric, baselinesheet, cols = 25, colNames = FALSE, startRow = 10)
      waterlostlength <- waterlostlength[waterlostlength$X1 != "", , drop = FALSE]
      waterlostlength <- waterlostlength[-nrow(waterlostlength), , drop = FALSE]
      waterlostlength$X1 <- as.numeric(waterlostlength$X1)
      colnames(waterlostlength) <- "lengthlost"
      
      #LostUnits
      waterlostunits<-openxlsx::read.xlsx(metric, baselinesheet, cols = 26, colNames = FALSE, startRow = 10)
      waterlostunits <- waterlostunits[waterlostunits$X1 != "", , drop = FALSE]
      waterlostunits <- waterlostunits[-nrow(waterlostunits), , drop = FALSE]
      waterlostunits$X1 <- as.numeric(waterlostunits$X1)
      colnames(waterlostunits) <- "lbulost"
      
      waterlostdata<-data.frame(waterhabitattype, waterlostlength, waterlostunits)
      
      #remove rows where both are 0 cus not retained
      waterlostdata <- waterlostdata[!(waterlostdata$lengthlost == 0 & waterlostdata$lbulost == 0), ]
      
      #if youre left with nothing, insert placeholder
      if (nrow(waterlostdata) == 0) {waterlostdata <- data.frame(habitattype = "No Watercourses Lost")}
      
    } else {
      
      waterlostdata<-data.frame(waterhabitattype)
      
    }
    
    waterlossdata<-list(waterlostdata = waterlostdata)
  
    return(waterlossdata)
  
}

#' pull created water data
#'
#' @param metric metric
#'
#' @return watercreationdata
#' @export
#'
#' @examples \dontrun{watercreation<-pullonsitewatercreation(metric)}
pullonsitewatercreation<-function(metric){
  
    #creationsheet
    creationsheet <- "C-2 On-Site WaterC' Creation"
    
    #Broad Habitat
    waterhabitattype<- openxlsx::read.xlsx(metric, creationsheet, cols = 3, colNames = FALSE, startRow = 12)
    
    #if nothing new created, insert placeholder so code doesnt crash
    if(is.null(waterhabitattype)) {
      waterhabitattype <- data.frame(habitattype = "No Watercourses Created") 
    } else {
      colnames(waterhabitattype) <- "habitattype"
    }
    
    #now run rest as long as there is data
    if (!"No Watercourses Created" %in% waterhabitattype){
      
      waternum<-openxlsx::read.xlsx(metric, creationsheet, cols = 29, colNames = FALSE, startRow = 12)
      colnames(waternum) <- "waternumber"  
      
      #Length
      waterlength<- openxlsx::read.xlsx(metric, creationsheet, cols = 4, colNames = FALSE, startRow = 12)
      waterlength<-waterlength[1:(nrow(waterlength) -1),, drop = FALSE]
      colnames(waterlength) <- "createdlength"
      
      #Condition
      watercondition <- openxlsx::read.xlsx(metric, creationsheet, cols = 7, colNames = FALSE, startRow = 12)
      colnames(watercondition) <- "createdcondition"
      
      #StrategicSignificance
      waterss <-openxlsx::read.xlsx(metric, creationsheet, cols = 9, colNames = FALSE, startRow = 12) 
      waterss$X1 <- ifelse(waterss$X1 == "Area/compensation not in local strategy/ no local strategy", "Low", waterss$X1)
      waterss$X1 <- ifelse(waterss$X1 == "Location ecologically desirable but not in local strategy", "Medium", waterss$X1)
      waterss$X1 <- ifelse(waterss$X1 == "Formally identified in local strategy", "High", waterss$X1)
      colnames(waterss) <- "createdss"
      
      #distinctiveness
      waterds<-openxlsx::read.xlsx(metric, creationsheet, cols = 5, colNames = FALSE, startRow = 12)
      waterds<- waterds[waterds$X1!= "", , drop = FALSE]
      waterds$X1 <- ifelse(waterds$X1 == "V.low", "Very Low", waterds$X1)
      waterds$X1 <- ifelse(waterds$X1 == "V.Low", "Very Low", waterds$X1)
      waterds$X1 <- ifelse(waterds$X1 == "V.high", "Very High", waterds$X1)
      colnames(waterds) <- "distinctiveness"
      
      #creationUnits
      waterunits<- openxlsx::read.xlsx(metric, creationsheet, cols = 26, colNames = FALSE, startRow = 12) 
      waterunits <- waterunits[waterunits$X1 != "", , drop = FALSE]
      waterunits <- waterunits[-nrow(waterunits), , drop = FALSE]
      waterunits$X1 <- as.numeric(waterunits$X1)
      colnames(waterunits) <- "createdunits"
      
      watercreationdata<-data.frame(waternum, waterhabitattype, waterlength, watercondition,
                                    waterss, waterds, waterunits)
      
    } else {
      
      watercreationdata <- data.frame(waterhabitattype)
      
    }
    
    #TotalwaterLength
    totalwaterlength <- openxlsx::read.xlsx(metric, creationsheet, cols = 4, colNames = FALSE, startRow = 260)
    colnames(totalwaterlength) <- "TotalCreatedwaterLength"
    
    #TotalwaterUnits
    totalwaterunits<-openxlsx::read.xlsx(metric, creationsheet, cols = 26, colNames = FALSE, startRow = 260)
    colnames(totalwaterunits) <- "TotalCreatedwaterUnits"
    
    watercreationdata<-list(watercreationdata = watercreationdata,
                            TotalCreatedwaterLength = totalwaterlength,
                            TotalCreatedwaterUnits = totalwaterunits)
    
    
    return(watercreationdata)
    }
  
#' pull water enhance
#'
#' @param metric metric
#'
#' @return ehnacne water data
#' @export
#'
#' @examples \dontrun{waterenhancementdata<-pullonsitewaterenhancement(metric)}
pullonsitewaterenhancement<-function(metric){
    #enhancement sheet
    enhancementsheet <- "C-3 On-Site WaterC' Enhancement"
    
    #hab type
    waterhabitattype <- openxlsx::read.xlsx(metric, sheet = enhancementsheet, cols = 14, colNames = FALSE, startRow = 12, skipEmptyRows = TRUE)
    
    #if nothing new created, insert placeholder so code doesnt crash
    if(is.null(waterhabitattype)) {
      waterhabitattype <- data.frame(habitattype = "No Watercourses Enhanced") 
    } else {
      colnames(waterhabitattype) <- "habitattype"
    }
    
    #now run rest as long as there is data
    if (!"No Watercourses Enhanced" %in% waterhabitattype){
      
      #length
      waterlength <- openxlsx::read.xlsx(metric, sheet = enhancementsheet, cols = 17, colNames = FALSE, startRow = 12, skipEmptyRows = TRUE)
      waterlength <- waterlength[waterlength$X1 != "", , drop = FALSE]
      waterlength <- waterlength[-nrow(waterlength), , drop = FALSE]
      waterlength$X1 <- as.numeric(waterlength$X1)
      colnames(waterlength) <- "enhancedlength"
      
      #basehab
      basehab <- openxlsx::read.xlsx(metric, sheet = enhancementsheet, cols = 3, colNames = FALSE, startRow = 12, skipEmptyRows = TRUE)
      basehab <- basehab[basehab$X1 != "", , drop = FALSE]
      colnames(basehab) <- "basehabitattype"
      
      #basecond
      basecond <- openxlsx::read.xlsx(metric, sheet = enhancementsheet, cols = 7, colNames = FALSE, startRow = 12, skipEmptyRows = TRUE)
      basecond <- basecond[basecond$X1 != "", , drop = FALSE]
      colnames(basecond) <- "basecondition"
      
      #condition
      watercond <- openxlsx::read.xlsx(metric, sheet = enhancementsheet, cols = 20, colNames = FALSE, startRow = 12, skipEmptyRows = TRUE)
      colnames(watercond) <- "enhancedcond"
      
      #ss
      waterss <- openxlsx::read.xlsx(metric, sheet = enhancementsheet, cols = 22, colNames = FALSE, startRow = 12, skipEmptyRows = TRUE)
      waterss$X1 <- ifelse(waterss$X1 == "Area/compensation not in local strategy/ no local strategy", "Low", waterss$X1)
      waterss$X1 <- ifelse(waterss$X1 == "Location ecologically desirable but not in local strategy", "Medium", waterss$X1)
      waterss$X1 <- ifelse(waterss$X1 == "Formally identified in local strategy", "High", waterss$X1)
      colnames(waterss) <- "enhancedss"
      
      #ds
      waterds <-openxlsx::read.xlsx(metric, sheet = enhancementsheet, cols = 18, colNames = FALSE, startRow = 12, skipEmptyRows = TRUE)
      waterds<- waterds[waterds$X1!= "", , drop = FALSE]
      waterds$X1 <- ifelse(waterds$X1 == "V.low", "Very Low", waterds$X1)
      waterds$X1 <- ifelse(waterds$X1 == "V.Low", "Very Low", waterds$X1)
      waterds$X1 <- ifelse(waterds$X1 == "V.high", "Very High", waterds$X1)
      waterds$X1 <- ifelse(waterds$X1 == "V.High", "Very High", waterds$X1)
      colnames(waterds) <- "distinctiveness"
      
      #units
      waterunits <- openxlsx::read.xlsx(metric, sheet = enhancementsheet, cols = 39, colNames = FALSE, startRow = 12, skipEmptyRows = TRUE)
      waterunits <- waterunits[waterunits$X1 != "", , drop = FALSE]
      waterunits <- waterunits[-nrow(waterunits), , drop = FALSE]
      waterunits$X1 <- as.numeric(waterunits$X1)
      colnames(waterunits) <- "enhancedunits"
      
      waterenhancementdata<-data.frame(waterhabitattype, waterlength, watercond,
                                       waterss, waterds, waterunits, basehab, basecond)
      
    } else {
      
      waterenhancementdata <- data.frame(waterhabitattype)
      
    }
    
    #add total enhanced length and units too
    
    waterenhancementdata <- list(waterenhancementdata = waterenhancementdata)
  
  return(waterenhancementdata)
}  

