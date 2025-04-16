#' pull onsite baseline data for summary
#'
#' @param metric feas metric
#'
#' @return a list, containing dataframes pulled from the baseline sheet of the metric
#'
#' @export
#'
#' @examples \dontrun{baselinedata<-pullA1baseline(metric)}
pullA1baseline<-function(metric){
 
  df <- openxlsx::read.xlsx(metric, "A-1 On-Site Habitat Baseline", cols = c(5:6, 8:9, 11, 13, 17:25), colNames = TRUE, startRow = 10)
  df <- df[, -c(8:13)]
  
  #read the dataset
  #df <- openxlsx::read.xlsx(metric, "A-1 On-Site Habitat Baseline", cols = c(5:6, 8:9, 11, 13, 17:18), colNames = TRUE, startRow = 10)
  
  #remove 'xml:space="preserve">' from column names
  colnames(df) <- gsub('xml:space="preserve">', '', colnames(df))
  
  #pull totals now, before slicing df
  totalarea<-as.data.frame(as.numeric(df$`Area.(hectares)`[249]))
  colnames(totalarea) <- "totalarea"
  totalunits<-as.data.frame(as.numeric(df$Total.habitat.units[249]))
  colnames(totalunits) <- "totalbaselineabu"
  
  #sometimes, there are entire rows of NAs. for the baseline sheet, you can remove all rows where col 1 = NA, if someone hasnt put broad hab in, wont exist
  df <- df[!is.na(df[, 1]), ]
  
  colnames(df) <- c("broadhabitat", "habitattype", "baselinearea", "distinctiveness", "baselinecondition", "baseliness", "baselineabu")
   
  #make numerical cols numeric
  df[, c(3, 7)] <- lapply(df[, c(3, 7)], function(x) round(as.numeric(x), 3))
  
  #assign SS correctly
  df$baseliness <- ifelse(df$baseliness == "Area/compensation not in local strategy/ no local strategy", "Low", df$baseliness)
  df$baseliness <- ifelse(df$baseliness == "Location ecologically desirable but not in local strategy", "Medium", df$baseliness)
  df$baseliness <- ifelse(df$baseliness == "Formally identified in local strategy", "High", df$baseliness)
  
  #assign distinctiveness correctly
  df$distinctiveness <- ifelse(df$distinctiveness == "V.low", "Very Low", df$distinctiveness)
  df$distinctiveness <- ifelse(df$distinctiveness == "V.Low", "Very Low", df$distinctiveness)
  df$distinctiveness <- ifelse(df$distinctiveness == "V.high", "Very High", df$distinctiveness)
  
  habitatbaselinedata<-list(habitatbaselinedata = df,
                            totalarea = totalarea,
                            totalunits = totalunits)
  
  return(habitatbaselinedata)

}

#' pull A1 retain
#'
#' @param metric feas metric
#'
#' @return a list, containing dataframes pulled from the baseline sheet of the metric
#'
#' @export
#'
#' @examples \dontrun{retaindata<-pullA1retain(metric)}
pullA1retain<-function(metric){
  
  df <- openxlsx::read.xlsx(metric, "A-1 On-Site Habitat Baseline", cols = c(5:6, 8:9, 11, 13, 17:25), colNames = TRUE, startRow = 10)
  df <- df[, -c(3:7, 9, 11:13)]
  
  #read the dataset
  #df <- openxlsx::read.xlsx(metric, "A-1 On-Site Habitat Baseline", cols = c(5:6,19,21), colNames = TRUE, startRow = 10)
  
  #remove 'xml:space="preserve">' from column names
  colnames(df) <- gsub('xml:space="preserve">', '', colnames(df))
  
  #pull totals now, before slicing df
  totalretainarea<-as.data.frame(as.numeric(df$Area.retained[249]))
  colnames(totalretainarea) <- "totalarearetained"
  totalretainunits<-as.data.frame(as.numeric(df$Baseline.units.retained[249]))
  colnames(totalretainunits) <- "totalaburetained"
  
  #sometimes, there are entire rows of NAs. for the baseline sheet, you can remove all rows where col 1 = NA, if someone hasnt put broad hab in, wont exist
  df <- df[!is.na(df[, 1]), ]
  
  #as its retaining, remove any where area retained = NA
  df <- df[!is.na(df[3]), ]
  
  colnames(df) <- c("broadhabitat", "habitattype", "arearetained", "aburetained")
  
  #make numerical cols numeric
  df$arearetained <-as.numeric(df$arearetained)
  
  retainhabitatdata<-list(habitatretaindata = df,
                          TotalRetainArea = totalretainarea,
                          TotalRetainUnits = totalretainunits)
  
  return(retainhabitatdata)
  
}

#' pull A1 loss
#'
#' @param metric feas metric
#'
#' @return a list, containing dataframes pulled from the baseline sheet of the metric
#'
#' @export
#'
#' @examples \dontrun{lostdata<-pullA1loss(metric)}
pullA1loss<-function(metric){
  
  df <- openxlsx::read.xlsx(metric, "A-1 On-Site Habitat Baseline", cols = c(5:6, 8:9, 11, 13, 17:25), colNames = TRUE, startRow = 10)
  df <- df[, -c(3:11)]
  
  #remove 'xml:space="preserve">' from column names
  colnames(df) <- gsub('xml:space="preserve">', '', colnames(df))
  
  #pull totals now, before slicing df
  totallostarea<-as.data.frame(as.numeric(df$Area.habitat.lost[249]))
  colnames(totallostarea) <- "totalarealost"
  totallostunits<-as.data.frame(as.numeric(df$Units.lost[249]))
  colnames(totallostunits) <- "totalabulost"
  
  #sometimes, there are entire rows of NAs. for the baseline sheet, you can remove all rows where col 1 = NA, if someone hasnt put broad hab in, wont exist
  df <- df[!is.na(df[, 1]), ]
  
  #as its retaining, remove any where area lost = 0
  df <- df[!(df[3] == "0"), ]
  
  colnames(df) <- c("broadhabitat", "habitattype", "arealost", "abulost")
  
  #make numerical cols numeric
  df$arealost <-as.numeric(df$arealost)
  
  lostdata<-list(habitatlostdata = df,
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
#' @examples \dontrun{creationdata<-pullA2(metric)}
pullA2<-function(metric){
  
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "A-2 On-Site Habitat Creation", cols = c(4:5, 7:8,10,12,25), colNames = TRUE, startRow = 10, skipEmptyRows = TRUE)
  
  #remove 'xml:space="preserve">' from column names
  colnames(df) <- gsub('xml:space="preserve">', '', colnames(df))
  
  #pull totals now, before slicing df
  totalcreationarea<-as.data.frame(as.numeric(df$X3[247]))
  colnames(totalcreationarea) <- "totalareacreated"
  totalcreationunits<-as.data.frame(as.numeric(df$X7[247]))
  colnames(totalcreationunits) <- "totalabucreated"
  
  #sometimes, there are entire rows of NAs. for the baseline sheet, you can remove all rows where col 1 = NA, if someone hasnt put broad hab in, wont exist
  df <- df[!is.na(df[, 1]), ]
  
  colnames(df) <- c("broadhabitat", "habitattype", "createdarea", "distinctiveness", "createdcondition", "createdss", "createdabu")
  
  #make numerical cols numeric
  df$createdarea <-as.numeric(df$createdarea)
  df$createdabu <-as.numeric(df$createdabu)
  
  #assign SS correctly
  df$createdss <- ifelse(df$createdss == "Area/compensation not in local strategy/ no local strategy", "Low", df$createdss)
  df$createdss <- ifelse(df$createdss == "Location ecologically desirable but not in local strategy", "Medium", df$createdss)
  df$createdss <- ifelse(df$createdss == "Formally identified in local strategy", "High", df$createdss)
  
  #assign distinctiveness correctly
  df$distinctiveness <- ifelse(df$distinctiveness == "V.low", "Very Low", df$distinctiveness)
  df$distinctiveness <- ifelse(df$distinctiveness == "V.Low", "Very Low", df$distinctiveness)
  df$distinctiveness <- ifelse(df$distinctiveness == "V.high", "Very High", df$distinctiveness)
  
  if (nrow(df) == 0){
    df <- data.frame(broadhabitat =  "No Habitats Created",
                     habitattype = "No Habitats Created")
  }
 
  habitatcreationdata <- list(habitatcreationdata = df,
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
#' @examples \dontrun{enhancedata<-pullA3(metric)}
pullA3<-function(metric){
  
  df <- openxlsx::read.xlsx(metric, sheet = "A-3 On-Site Habitat Enhancement", 
                            cols = c(6,17:18,22:23,25, 27,40), 
                            colNames = TRUE, startRow = 10)
  
  #remove 'xml:space="preserve">' from column names
  colnames(df) <- gsub('xml:space="preserve">', '', colnames(df))
  
  #pull totals now, before slicing df
  totalenhancearea<-as.data.frame(as.numeric(df$X3[247]))
  colnames(totalenhancearea) <- "totalareaenhanced"
  totalenhanceunits<-as.data.frame(as.numeric(df$X7[247]))
  colnames(totalenhanceunits) <- "totalabuenhanced"
  
  #sometimes, there are entire rows of NAs. for the baseline sheet, you can remove all rows where col 3 = NA, if someone hasnt put broad hab in, wont exist
  df <- df[!is.na(df[, 3]), ]
  
  colnames(df) <- c("basehabitattype","broadhabitat", "habitattype", "enhancedarea", "distinctiveness", "enhancedcondition", "enhancedss", "enhancedabu")
  
  df <- df[-1, ] #get rid of first row, as column titles in
  
  #make numerical cols numeric
  df$enhancedarea <-as.numeric(df$enhancedarea)
  df$enhancedabu <-as.numeric(df$enhancedabu)
  
  #assign SS correctly
  df$enhancedss <- ifelse(df$enhancedss == "Area/compensation not in local strategy/ no local strategy", "Low", df$enhancedss)
  df$enhancedss <- ifelse(df$enhancedss == "Location ecologically desirable but not in local strategy", "Medium", df$enhancedss)
  df$enhancedss <- ifelse(df$enhancedss == "Formally identified in local strategy", "High", df$enhancedss)
  
  #assign distinctiveness correctly
  df$distinctiveness <- ifelse(df$distinctiveness == "V.low", "Very Low", df$distinctiveness)
  df$distinctiveness <- ifelse(df$distinctiveness == "V.Low", "Very Low", df$distinctiveness)
  df$distinctiveness <- ifelse(df$distinctiveness == "V.high", "Very High", df$distinctiveness)
  
  if (nrow(df) == 0){
    df <- data.frame(broadhabitat =  "No Habitats Enhanced",
                     habitattype = "No Habitats Enhanced")
  }
  
  enhancedata <- list(habitatenhancementdata = df,
                      TotalEnhanceArea = totalenhancearea,
                      TotalEnhanceUnits = totalenhanceunits)
  
  return(enhancedata)
  
  
}