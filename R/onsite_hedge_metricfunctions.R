#' pulls on site hedge baseline data
#'
#' @param metric metric
#'
#' @return hedgebaselinedata
#' @export
#'
#' @examples \dontrun{hedgebaselinedata<-pullonsitehedgebaseline(metric)}
pullonsitehedgebaseline<-function(metric){
  
  #baselinesheet
  baselinesheet <- "B-1 On-Site Hedge Baseline"
  
  #Broad Habitat
  hedgehabitattype<- openxlsx::read.xlsx(metric, baselinesheet, cols = 4, colNames = FALSE, startRow = 10)
  
  #if nothing on baseline, insert placeholder so code doesnt crash
  if(is.null(hedgehabitattype)) {
    hedgehabitattype <- data.frame(habitattype = "No Existing Hedgerows") 
  } else {
    colnames(hedgehabitattype) <- "habitattype"
  }
  
  #now run rest as long as there is data
  if (!"No Existing Hedgerows" %in% hedgehabitattype){
  
  #Length
  hedgelength<- openxlsx::read.xlsx(metric, baselinesheet, cols = 5, colNames = FALSE, startRow = 10)
  hedgelength<-hedgelength[1:(nrow(hedgelength) -1),, drop = FALSE]
  colnames(hedgelength) <- "baselinelength"
  
  #Condition
  hedgecondition <- openxlsx::read.xlsx(metric, baselinesheet, cols = 8, colNames = FALSE, startRow = 10)
  colnames(hedgecondition) <- "baselinecondition"
  
  #StrategicSignificance
  hedgess <-openxlsx::read.xlsx(metric, baselinesheet, cols = 10, colNames = FALSE, startRow = 10) 
  hedgess$X1 <- ifelse(hedgess$X1 == "Area/compensation not in local strategy/ no local strategy", "Low", hedgess$X1)
  hedgess$X1 <- ifelse(hedgess$X1 == "Location ecologically desirable but not in local strategy", "Medium", hedgess$X1)
  hedgess$X1 <- ifelse(hedgess$X1 == "Formally identified in local strategy", "High", hedgess$X1)
  colnames(hedgess) <- "baseliness"
  
  #BaselineUnits
  hedgeunits<- openxlsx::read.xlsx(metric, baselinesheet, cols = 14, colNames = FALSE, startRow = 10) 
  hedgeunits <- hedgeunits[hedgeunits$X1 != "", , drop = FALSE]
  hedgeunits <- hedgeunits[-nrow(hedgeunits), , drop = FALSE]
  hedgeunits$X1 <- as.numeric(hedgeunits$X1)
  colnames(hedgeunits) <- "baselinelbu"
  
  #hedgenumbers
  hedgeid<-openxlsx::read.xlsx(metric, baselinesheet, cols = 3, colNames = FALSE, startRow = 10)
  colnames(hedgeid)<-"baseid"
  
  hedgebaselinedata<-data.frame(hedgehabitattype, hedgelength, hedgecondition, hedgess, hedgeunits, hedgeid)}
  
  else {
    
    hedgebaselinedata<-data.frame(hedgehabitattype)
    
  }
  #EnhanceLengths
  hedgeenhancelength<-openxlsx::read.xlsx(metric, baselinesheet, cols = 17, colNames = FALSE, startRow = 10)
  colnames(hedgeenhancelength) <- "enhancedlength"
  
  #EnhanceUnits
  hedgeenhanceunits<-openxlsx::read.xlsx(metric, baselinesheet, cols = 19, colNames = FALSE, startRow = 10)
  hedgeenhanceunits <- hedgeenhanceunits[hedgeenhanceunits$X1 != "", , drop = FALSE]
  hedgeenhanceunits <- hedgeenhanceunits[-nrow(hedgeenhanceunits), , drop = FALSE]
  hedgeenhanceunits$X1 <- as.numeric(hedgeenhanceunits$X1)
  colnames(hedgeenhanceunits) <- "enhancedunits"
  
  #TotalHedgeLength
  totalhedgelength <- openxlsx::read.xlsx(metric, baselinesheet, cols = 5, colNames = FALSE, startRow = 258)
  totalhedgelength<-round(totalhedgelength, 2)
  colnames(totalhedgelength) <- "totallength"
  
  #TotalHedgeUnits
  totalhedgeunits<-openxlsx::read.xlsx(metric, baselinesheet, cols = 14, colNames = FALSE, startRow = 258)
  totalhedgeunits<-round(totalhedgeunits, 2)
  colnames(totalhedgeunits) <- "totallbu"
  
  hedgebaselinedata<-list(hedgebaselinedata = hedgebaselinedata,
                     LengthEnhanced = hedgeenhancelength, #can remove once function written
                     UnitsEnhanced = hedgeenhanceunits,
                     totallength = totalhedgelength,
                     totalunits = totalhedgeunits)
                      
  return(hedgebaselinedata)
  
}

#' pullhedgeretain
#'
#' @param metric metric
#'
#' @return retain hedge data
#' @export
#'
#' @examples \dontrun{hedgeretaindata<-pullonsitehedgeretain(metric)}
pullonsitehedgeretain<-function(metric){
  
  #baselinesheet
  baselinesheet <- "B-1 On-Site Hedge Baseline"
  
  #Broad Habitat
  hedgehabitattype<- openxlsx::read.xlsx(metric, baselinesheet, cols = 4, colNames = FALSE, startRow = 10)
  
  #if nothing on baseline, insert placeholder so code doesnt crash
  if(is.null(hedgehabitattype)) {
    hedgehabitattype <- data.frame(habitattype = "No Existing Hedgerows") 
  } else {
    colnames(hedgehabitattype) <- "habitattype"
  
  #hedgenumbers
  hedgenum<-openxlsx::read.xlsx(metric, baselinesheet, cols = 3, colNames = FALSE, startRow = 10)
  colnames(hedgenum) <- "hedgenumber"
  
  #RetainLengths
  hedgeretainlength<-openxlsx::read.xlsx(metric, baselinesheet, cols = 16, colNames = FALSE, startRow = 10)
  if (nrow(hedgeretainlength) >=2) {
    hedgeretainlength<-hedgeretainlength[-nrow(hedgeretainlength),]
    hedgeretainlength<-as.data.frame(hedgeretainlength)
    }
  colnames(hedgeretainlength) <- "lengthretained"
  
  #RetainUnits
  hedgeretainunits<-openxlsx::read.xlsx(metric, baselinesheet, cols = 18, colNames = FALSE, startRow = 10)
  hedgeretainunits <- hedgeretainunits[hedgeretainunits$X1 != "", , drop = FALSE]
  hedgeretainunits <- hedgeretainunits[-nrow(hedgeretainunits), , drop = FALSE]
  hedgeretainunits$X1 <- as.numeric(hedgeretainunits$X1)
  colnames(hedgeretainunits) <- "lburetained"
  
  hedgeretaindata<-data.frame(hedgenum, hedgehabitattype, hedgeretainlength, hedgeretainunits)
  #remove rows where both are 0 cus not retained
  hedgeretaindata <- hedgeretaindata[!(hedgeretaindata$lengthretained == 0 & hedgeretaindata$lburetained == 0), ]
  
  #if youre left with nothing, insert placeholder
  if (nrow(hedgeretaindata) == 0) {hedgeretaindata <- data.frame(habitattype = "No Hedgerows Retained")}
  
  }
  
  hedgeretaindata<-list(hedgeretaindata = hedgeretaindata)
  
  return(hedgeretaindata)
  
}

#' pullhedgeloss
#'
#' @param metric metric
#'
#' @return lost hedge data
#' @export
#'
#' @examples \dontrun{hedgelossdata<-pullonsitehedgeloss(metric)}
pullonsitehedgeloss<-function(metric){
  
  #baselinesheet
  baselinesheet <- "B-1 On-Site Hedge Baseline"
  
  #Broad Habitat
  hedgehabitattype<- openxlsx::read.xlsx(metric, baselinesheet, cols = 4, colNames = FALSE, startRow = 10)
  
  #if nothing on baseline, insert placeholder so code doesnt crash
  if(is.null(hedgehabitattype)) {
    
    hedgehabitattype <- data.frame(habitattype = "No Existing Hedgerows")
    
  } else {
    
  colnames(hedgehabitattype) <- "habitattype"
  
  hedgenum<-openxlsx::read.xlsx(metric, baselinesheet, cols = 3, colNames = FALSE, startRow = 10)
  colnames(hedgenum) <- "hedgenumber"
  
  #LostLength
  hedgelostlength <-openxlsx::read.xlsx(metric, baselinesheet, cols = 20, colNames = FALSE, startRow = 10)
  hedgelostlength <- hedgelostlength[hedgelostlength$X1 != "", , drop = FALSE]
  hedgelostlength <- hedgelostlength[-nrow(hedgelostlength), , drop = FALSE]
  hedgelostlength$X1 <- as.numeric(hedgelostlength$X1)
  colnames(hedgelostlength) <- "lengthlost"
  
  #LostUnits
  hedgelostunits<-openxlsx::read.xlsx(metric, baselinesheet, cols = 21, colNames = FALSE, startRow = 10)
  hedgelostunits <- hedgelostunits[hedgelostunits$X1 != "", , drop = FALSE]
  hedgelostunits <- hedgelostunits[-nrow(hedgelostunits), , drop = FALSE]
  hedgelostunits$X1 <- as.numeric(hedgelostunits$X1)
  colnames(hedgelostunits) <- "lbulost"
  
  hedgelostdata<-data.frame(hedgenum, hedgehabitattype, hedgelostlength, hedgelostunits)
  #remove rows where both are 0 cus not retained
  hedgelostdata <- hedgelostdata[!(hedgelostdata$lengthlost == 0 & hedgelostdata$lbulost == 0), ]
    
  #if youre left with nothing, insert placeholder
  if (nrow(hedgelostdata) == 0) {hedgelostdata <- data.frame(habitattype = "No Hedgerows Lost")}
  
  }

  hedgelossdata<-list(hedgelostdata = hedgelostdata)
  
  return(hedgelossdata)
  
}

#' pull created hedge data
#'
#' @param metric metric
#'
#' @return hedgecreationdata
#' @export
#'
#' @examples \dontrun{hedgecreation<-pullonsitehedgecreation(metric)}
pullonsitehedgecreation<-function(metric){
  
  #creationsheet
  creationsheet <- "B-2 On-Site Hedge Creation"
  
  #Broad Habitat
  hedgehabitattype<- openxlsx::read.xlsx(metric, creationsheet, cols = 4, colNames = FALSE, startRow = 12)
  
  #if nothing new created, insert placeholder so code doesnt crash
  if(is.null(hedgehabitattype)) {
    hedgehabitattype <- data.frame(habitattype = "No Hedgerows Created") 
  } else {
    colnames(hedgehabitattype) <- "habitattype"
    }
  
  #now run rest as long as there is data
  if (!"No Hedgerows Created" %in% hedgehabitattype){
  
  hedgenum<-openxlsx::read.xlsx(metric, creationsheet, cols = 3, colNames = FALSE, startRow = 12)
  colnames(hedgenum) <- "hedgenumber"  
    
  #Length
  hedgelength<- openxlsx::read.xlsx(metric, creationsheet, cols = 5, colNames = FALSE, startRow = 12)
  hedgelength<-hedgelength[1:(nrow(hedgelength) -1),, drop = FALSE]
  colnames(hedgelength) <- "createdlength"
  
  #Condition
  hedgecondition <- openxlsx::read.xlsx(metric, creationsheet, cols = 8, colNames = FALSE, startRow = 12)
  colnames(hedgecondition) <- "createdcondition"
  
  #StrategicSignificance
  hedgess <-openxlsx::read.xlsx(metric, creationsheet, cols = 10, colNames = FALSE, startRow = 12) 
  hedgess$X1 <- ifelse(hedgess$X1 == "Area/compensation not in local strategy/ no local strategy", "Low", hedgess$X1)
  hedgess$X1 <- ifelse(hedgess$X1 == "Location ecologically desirable but not in local strategy", "Medium", hedgess$X1)
  hedgess$X1 <- ifelse(hedgess$X1 == "Formally identified in local strategy", "High", hedgess$X1)
  colnames(hedgess) <- "createdss"
  
  #distinctiveness
  hedgeds<-openxlsx::read.xlsx(metric, creationsheet, cols = 6, colNames = FALSE, startRow = 12)
  hedgeds<- hedgeds[hedgeds$X1!= "", , drop = FALSE]
  hedgeds$X1 <- ifelse(hedgeds$X1 == "V.low", "Very Low", hedgeds$X1)
  hedgeds$X1 <- ifelse(hedgeds$X1 == "V.Low", "Very Low", hedgeds$X1)
  hedgeds$X1 <- ifelse(hedgeds$X1 == "V.high", "Very High", hedgeds$X1)
  colnames(hedgeds) <- "distinctiveness"
  
  #creationUnits
  hedgeunits<- openxlsx::read.xlsx(metric, creationsheet, cols = 23, colNames = FALSE, startRow = 12) 
  hedgeunits <- hedgeunits[hedgeunits$X1 != "", , drop = FALSE]
  hedgeunits <- hedgeunits[-nrow(hedgeunits), , drop = FALSE]
  hedgeunits$X1 <- as.numeric(hedgeunits$X1)
  colnames(hedgeunits) <- "createdunits"
  
  hedgecreationdata<-data.frame(hedgenum, hedgehabitattype, hedgelength, hedgecondition,
                          hedgess, hedgeds, hedgeunits)
  
  } else {
   
    hedgecreationdata <- data.frame(hedgehabitattype)
     
  }
  
  #TotalHedgeLength
  totalhedgelength <- openxlsx::read.xlsx(metric, creationsheet, cols = 5, colNames = FALSE, startRow = 260)
  colnames(totalhedgelength) <- "TotalCreatedHedgeLength"
  
  #TotalHedgeUnits
  totalhedgeunits<-openxlsx::read.xlsx(metric, creationsheet, cols = 23, colNames = FALSE, startRow = 260)
  colnames(totalhedgeunits) <- "TotalCreatedHedgeUnits"
  
  hedgecreationdata<-list(hedgecreationdata = hedgecreationdata,
                        TotalCreatedHedgeLength = totalhedgelength,
                        TotalCreatedHedgeUnits = totalhedgeunits)
  
  
  return(hedgecreationdata)
  
}

#' pull hedge enhance
#'
#' @param metric metric
#'
#' @return ehnacne hedge data
#' @export
#'
#' @examples \dontrun{hedgeenhancementdata<-pullonsitehedgeenhancement(metric)}
pullonsitehedgeenhancement <- function(metric) {
  # Define the enhancement sheet name
  enhancementsheet <- "B-3 On-Site Hedge Enhancement"
  
  # Helper function to read data safely
  safe_read <- function(sheet, cols) {
    tryCatch(
      {
        data <- openxlsx::read.xlsx(
          metric,
          sheet = sheet,
          cols = cols,
          colNames = FALSE,
          startRow = 12,
          skipEmptyRows = TRUE
        )
        # Remove empty rows and ensure there's data
        if (!is.null(data) && nrow(data) > 0) {
          data <- data[data$X1 != "", , drop = FALSE]
          if (nrow(data) > 0) return(data)
        }
        NULL
      },
      error = function(e) NULL
    )
  }
  
  # Read hedge habitat type
  hedgehabitattype <- safe_read(enhancementsheet, 13)
  
  if (is.null(hedgehabitattype) || nrow(hedgehabitattype) == 0) {
    hedgehabitattype <- data.frame(habitattype = "No Hedgerows Enhanced")
  } else {
    hedgehabitattype <- hedgehabitattype[-nrow(hedgehabitattype), , drop = FALSE]
    hedgehabitattype$X1 <- sub(".*- ", "", hedgehabitattype$X1)
    colnames(hedgehabitattype) <- "habitattype"
  }
  
  # If no data, return placeholder
  if ("No Hedgerows Enhanced" %in% hedgehabitattype$habitattype) {
    return(list(hedgeenhancementdata = data.frame(hedgehabitattype)))
  }
  
  # Read additional columns safely
  hedgelength <- safe_read(enhancementsheet, 16)
  basehab <- safe_read(enhancementsheet, 3)
  basecond <- safe_read(enhancementsheet, 7)
  hedgecond <- safe_read(enhancementsheet, 19)
  hedgess <- safe_read(enhancementsheet, 22)
  hedgeds <- safe_read(enhancementsheet, 17)
  hedgeunits <- safe_read(enhancementsheet, 34)
  
  # Process each dataset if it exists
  if (!is.null(hedgelength)) {
    hedgelength <- hedgelength[-nrow(hedgelength), , drop = FALSE]
    hedgelength$X1 <- as.numeric(hedgelength$X1)
    colnames(hedgelength) <- "enhancedlength"
  } else {
    hedgelength <- data.frame(enhancedlength = NA)
  }
  
  if (!is.null(basehab)) {
    colnames(basehab) <- "basehabitattype"
  } else {
    basehab <- data.frame(basehabitattype = NA)
  }
  
  if (!is.null(basecond)) {
    colnames(basecond) <- "basecondition"
  } else {
    basecond <- data.frame(basecondition = NA)
  }
  
  if (!is.null(hedgecond)) {
    hedgecond <- hedgecond[-nrow(hedgecond), , drop = FALSE]
    colnames(hedgecond) <- "enhancedcond"
  } else {
    hedgecond <- data.frame(enhancedcond = NA)
  }
  
  if (!is.null(hedgess)) {
    hedgess <- hedgess[-nrow(hedgess), , drop = FALSE]
    hedgess$X1 <- ifelse(hedgess$X1 == "Area/compensation not in local strategy/ no local strategy", "Low", hedgess$X1)
    hedgess$X1 <- ifelse(hedgess$X1 == "Location ecologically desirable but not in local strategy", "Medium", hedgess$X1)
    hedgess$X1 <- ifelse(hedgess$X1 == "Formally identified in local strategy", "High", hedgess$X1)
    colnames(hedgess) <- "enhancedss"
  } else {
    hedgess <- data.frame(enhancedss = NA)
  }
  
  if (!is.null(hedgeds)) {
    hedgeds <- hedgeds[-nrow(hedgeds), , drop = FALSE]
    hedgeds$X1 <- ifelse(hedgeds$X1 == "V.low" | hedgeds$X1 == "V.Low", "Very Low", hedgeds$X1)
    hedgeds$X1 <- ifelse(hedgeds$X1 == "V.high", "Very High", hedgeds$X1)
    colnames(hedgeds) <- "distinctiveness"
  } else {
    hedgeds <- data.frame(distinctiveness = NA)
  }
  
  if (!is.null(hedgeunits)) {
    hedgeunits <- hedgeunits[-nrow(hedgeunits), , drop = FALSE]
    hedgeunits$X1 <- as.numeric(hedgeunits$X1)
    colnames(hedgeunits) <- "enhancedunits"
  } else {
    hedgeunits <- data.frame(enhancedunits = NA)
  }
  
  # Combine data into a final data frame
  hedgeenhancementdata <- data.frame(
    hedgehabitattype,
    hedgelength,
    hedgecond,
    hedgess,
    hedgeds,
    hedgeunits,
    basehab,
    basecond
  )
  
  # Return as a list
  return(list(hedgeenhancementdata = hedgeenhancementdata))
}

# pullonsitehedgeenhancement<-function(metric){
#   
#   #enhancement sheet
#   enhancementsheet <- "B-3 On-Site Hedge Enhancement"
#   
#   #hab type  # CHECK WHICH ONE IS SUPPOSED TO BE READ IN HERE e.g pre-or post habitat type
#   hedgehabitattype <- openxlsx::read.xlsx(metric, sheet = enhancementsheet, cols = 13, colNames = FALSE, startRow = 12, skipEmptyRows = TRUE)
#   hedgehabitattype<-hedgehabitattype[hedgehabitattype$X1 !="", , drop = FALSE]
#   hedgehabitattype <- hedgehabitattype[-nrow(hedgehabitattype), , drop = FALSE]
#   hedgehabitattype$X1 <- sub(".*- ", "", hedgehabitattype$X1)  
#   
#   #if nothing new created, insert placeholder so code doesnt crash
#   if(is.null(hedgehabitattype)) {
#     hedgehabitattype <- data.frame(habitattype = "No Hedgerows Enhanced") 
#   } else {
#     colnames(hedgehabitattype) <- "habitattype"
#   }
#   
#   #now run rest as long as there is data
#   if (!"No Hedgerows Enhanced" %in% hedgehabitattype){
#     
#     #length
#     hedgelength <- openxlsx::read.xlsx(metric, sheet = enhancementsheet, cols = 16, colNames = FALSE, startRow = 12, skipEmptyRows = TRUE)
#     hedgelength <- hedgelength[hedgelength$X1 != "", , drop = FALSE]
#     hedgelength <- hedgelength[-nrow(hedgelength), , drop = FALSE]
#     hedgelength$X1 <- as.numeric(hedgelength$X1)
#     colnames(hedgelength) <- "enhancedlength" 
#     
#     #basehab
#     basehab <- openxlsx::read.xlsx(metric, sheet = enhancementsheet, cols = 3, colNames = FALSE, startRow = 12, skipEmptyRows = TRUE)
#     basehab <- basehab[basehab$X1 != "", , drop = FALSE]
#     colnames(basehab) <- "basehabitattype"
#     
#     #basecond
#     basecond <- openxlsx::read.xlsx(metric, sheet = enhancementsheet, cols = 7, colNames = FALSE, startRow = 12, skipEmptyRows = TRUE)
#     basecond <- basecond[basecond$X1 != "", , drop = FALSE]
#     colnames(basecond) <- "basecondition"
#     
#     #condition
#     hedgecond <- openxlsx::read.xlsx(metric, sheet = enhancementsheet, cols = 19, colNames = FALSE, startRow = 12, skipEmptyRows = TRUE)
#     hedgecond <- hedgecond[-nrow(hedgecond), , drop = FALSE]
#     colnames(hedgecond) <- "enhancedcond"
#     
#     #ss
#     hedgess <- openxlsx::read.xlsx(metric, sheet = enhancementsheet, cols = 22, colNames = FALSE, startRow = 12, skipEmptyRows = TRUE)
#     hedgess <- hedgess[hedgess$X1 != "", , drop = FALSE]
#     hedgess$X1 <- ifelse(hedgess$X1 == "Area/compensation not in local strategy/ no local strategy", "Low", hedgess$X1)
#     hedgess$X1 <- ifelse(hedgess$X1 == "Location ecologically desirable but not in local strategy", "Medium", hedgess$X1)
#     hedgess$X1 <- ifelse(hedgess$X1 == "Formally identified in local strategy", "High", hedgess$X1)
#     hedgess <- hedgess[-nrow(hedgess), , drop = FALSE]
#     colnames(hedgess) <- "enhancedss"
#     
#     #ds
#     hedgeds <-openxlsx::read.xlsx(metric, sheet = enhancementsheet, cols = 17, colNames = FALSE, startRow = 12, skipEmptyRows = TRUE)
#     hedgeds<- hedgeds[hedgeds$X1!= "", , drop = FALSE]
#     hedgeds$X1 <- ifelse(hedgeds$X1 == "V.low", "Very Low", hedgeds$X1)
#     hedgeds$X1 <- ifelse(hedgeds$X1 == "V.Low", "Very Low", hedgeds$X1)
#     hedgeds$X1 <- ifelse(hedgeds$X1 == "V.high", "Very High", hedgeds$X1)
#     hedgeds <- hedgeds[-nrow(hedgeds), , drop = FALSE]
#     
#     colnames(hedgeds) <- "distinctiveness"
#     
#     #units
#     hedgeunits <- openxlsx::read.xlsx(metric, sheet = enhancementsheet, cols = 34, colNames = FALSE, startRow = 12, skipEmptyRows = TRUE)
#     hedgeunits <- hedgeunits[hedgeunits$X1 != "", , drop = FALSE]
#     hedgeunits <- hedgeunits[-nrow(hedgeunits), , drop = FALSE]
#     hedgeunits$X1 <- as.numeric(hedgeunits$X1)
#     colnames(hedgeunits) <- "enhancedunits"
#     
#     hedgeenhancementdata<-data.frame(hedgehabitattype, hedgelength, hedgecond,
#                                      hedgess, hedgeds, hedgeunits, basehab, basecond)
#     
#   } else {
#     
#     hedgeenhancementdata <- data.frame(hedgehabitattype)
#     
#   }
#   
#   #add total enhanced length and units too
#   
#   hedgeenhancementdata <- list(hedgeenhancementdata = hedgeenhancementdata)
#   
#   return(hedgeenhancementdata)
#   
# }