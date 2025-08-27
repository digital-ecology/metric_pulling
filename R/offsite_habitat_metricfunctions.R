
#' Offsite baseline habitat data for summary
#'
#' @param metric BNG metric
#'
#' @return a list, containing dataframes pulled from the offsite baseline sheet of the metric
#'
#' @export
#'
#' @examples \dontrun{baselinedata<-offsitehabitatbaseline(metric)}
offsitehabitatbaseline<-function(metric){
  
  basedf <- openxlsx::read.xlsx(metric,
                                sheet = "D-1 Off-Site Habitat Baseline",
                                cols = c(5,6,8,9,11,13,20),
                                rows = 11:258,
                                startRow = 11,
                                colNames = FALSE,
                                skipEmptyRows = TRUE)
  
  basedf <- na.omit(basedf)
  
  colnames(basedf) <- c("broadhabitat",
                        "habitattype",
                        "baselinearea",
                        "distinctiveness",
                        "baselinecondition",
                        "baseliness",
                        "baselineabu")
  
  basedf$distinctiveness <- ifelse(basedf$distinctiveness == "V.low", "Very Low", basedf$distinctiveness)
  basedf$distinctiveness <- ifelse(basedf$distinctiveness == "V.Low", "Very Low", basedf$distinctiveness)
  basedf$distinctiveness <- ifelse(basedf$distinctiveness == "V.high", "Very High", basedf$distinctiveness)
  
  basedf$baseliness <- ifelse(basedf$baseliness == "Area/compensation not in local strategy/ no local strategy", "Low", basedf$baseliness)
  basedf$baseliness <- ifelse(basedf$baseliness == "Location ecologically desirable but not in local strategy", "Medium", basedf$baseliness)
  basedf$baseliness <- ifelse(basedf$baseliness == "Formally identified in local strategy", "High", basedf$baseliness)

  basedf$baselineabu <- as.numeric(basedf$baselineabu)
  
  calc_total_area <- sum(basedf$baselinearea, na.rm = TRUE)
  
  calc_total_units <- sum(basedf$baselineabu, na.rm = TRUE)
  
  habitatbaselinedata<-list(habitatbaselinedata = basedf,
                            totalarea = calc_total_area,
                            totalunits = calc_total_units)
  
  
  return(habitatbaselinedata)
  
}


#' Offsite retained habitat data for summary
#'
#' @param metric A BNG metric
#'
#' @returns A list with a summary data.frame, an numeric for total area retained
#' and a numeric for total BNG units retained
#' @export
#'
#' @examples \dontrun{retaineddata<-offsitehabitatretain(metric)}
offsitehabitatretain <- function(metric){
  
  retaindf <- openxlsx::read.xlsx(metric,
                                  sheet = "D-1 Off-Site Habitat Baseline",
                                  cols = c(5, 6, 22, 24),
                                  rows = 11:258,
                                  startRow = 11,
                                  colNames = FALSE,
                                  skipEmptyRows = TRUE)
  
  retaindf <- na.omit(retaindf)
  
  colnames(retaindf) <- c("broadhabitat",
                          "habitattype",
                          "arearetained",
                          "aburetained")
  
  retaindf$aburetained <- as.numeric(retaindf$aburetained)
  
  if (nrow(retaindf) == 0) {retaindf<-data.frame(broadhabitat = "No Habitats Retained")}
  
  totalretainarea <- sum(retaindf$arearetained, na.rm = TRUE)
  totalretainunits <- sum(retaindf$aburetained, na.rm = TRUE)
  
  retainhabitatdata<-list(habitatretaindata = retaindf,
                          TotalRetainArea = totalretainarea,
                          TotalRetainUnits = totalretainunits)
  
  return(retainhabitatdata)
  
}


#' Off site habitats lost data
#'
#' @param metric A BNG metric
#'
#' @returns A list with a summary data.frame, an numeric for total habitat area 
#' lost and a numeric for total BNG units lost
#' @export
#'
#' @examples \dontrun{lossdata<-offsitehabitatloss(metric)}
offsitehabitatloss <- function(metric){
  
  lostdf <- openxlsx::read.xlsx(metric,
                                sheet = "D-1 Off-Site Habitat Baseline",
                                cols = c(5, 6, 26, 27),
                                rows = 11:258,
                                startRow = 11,
                                colNames = FALSE,
                                skipEmptyRows = TRUE)
  
  lostdf <- na.omit(lostdf)
  
  colnames(lostdf) <- c("broadhabitat",
                        "habitattype",
                        "arealost",
                        "abulost")
  
  lostdf$abulost <- as.numeric(lostdf$abulost)
  lostdf$arealost <- as.numeric(lostdf$arealost)
  
  #if (nrow(lostdf) == 0) {lostdf<-data.frame(broadhabitat = "No Habitats Lost")}
  
  #this just pulls all, so filter out areas where both are 0 as that means no area / units lost
  lostdf <- lostdf[!(lostdf$arealost == 0 & lostdf$abulost == 0), ]
  
  totallostarea <- sum(lostdf$arealost, na.rm = TRUE)
  totallostunits <- sum(lostdf$abulost, na.rm = TRUE)
  
  lostdata<-list(habitatlostdata = lostdf,
                 TotallostArea = totallostarea,
                 TotallostUnits = totallostunits)
  
  return(lostdata)
  
}


#' Offsite habitat creation data summary
#'
#' @param metric A BNG metric
#'
#' @returns A list with a summary data.frame, an numeric for total habitat area 
#' created and a numeric for total BNG units created
#' @export
#'
#' @examples \dontrun{creationdata <- offsitehabitatcreation(metric)}
offsitehabitatcreation <- function(metric){
  
  createdf <- openxlsx::read.xlsx(metric,
                                  sheet = "D-2 Off-Site Habitat Creation",
                                  cols = c(4, 5, 7, 8, 10, 12, 19, 28),
                                  rows = 11:256,
                                  startRow = 11,
                                  colNames = FALSE,
                                  skipEmptyRows = TRUE)
  
  createdf <- na.omit(createdf)
  
  colnames(createdf) <- c("broadhabitat",
                          "habitattype",
                          "createdarea",
                          "distinctiveness",
                          "createdcondition",
                          "createdss",
                          "createdtime",
                          "createdabu")
  
  createdf$createdabu <- as.numeric(createdf$createdabu)
  createdf$createdarea <- as.numeric(createdf$createdarea)
  
  createdf$createdss <- ifelse(createdf$createdss == "Area/compensation not in local strategy/ no local strategy", "Low", createdf$createdss)
  createdf$createdss <- ifelse(createdf$createdss == "Location ecologically desirable but not in local strategy", "Medium", createdf$createdss)
  createdf$createdss <- ifelse(createdf$createdss == "Formally identified in local strategy", "High", createdf$createdss)
  
  createdf$distinctiveness <- ifelse(createdf$distinctiveness == "V.low", "Very Low", createdf$distinctiveness)
  createdf$distinctiveness <- ifelse(createdf$distinctiveness == "V.Low", "Very Low", createdf$distinctiveness)
  createdf$distinctiveness <- ifelse(createdf$distinctiveness == "V.high", "Very High", createdf$distinctiveness)
  
  totalcreationarea <- round(sum(createdf$createdarea, na.rm = TRUE), 2)
  totalcreationunits <- round(sum(createdf$createdabu, na.rm = TRUE), 2)
  
  habitatcreationdata <- list(habitatcreationdata = createdf,
                              TotalCreationArea = totalcreationarea,
                              TotalCreationUnits = totalcreationunits)
  
  return(habitatcreationdata)
  
}


#' Off-site habitat enhancement summary data
#'
#' @param metric A BNG metric
#'
#' @returns A list with a summary data.frame, an numeric for total habitat area 
#' enhanced and a numeric for total BNG units enhanced
#' @export
#'
#' @examples \dontrun{enhancedata <- offsitehabitatenhance(metric)}
offsitehabitatenhance <- function(metric){
  
  enhancedf <- openxlsx::read.xlsx(metric,
                                  sheet = "D-3 Off-Site Habitat Enhancment",
                                  cols = c(17,18,22,23,25,27,34,42),
                                  rows = 12:258,
                                  startRow = 12,
                                  colNames = FALSE,
                                  skipEmptyRows = TRUE)
  
  enhancedf <- na.omit(enhancedf)
  
  colnames(enhancedf) <- c("broadhabitat",
                          "habitattype",
                          "enhancedarea",
                          "distinctiveness",
                          "enhancedcondition",
                          "enhancedss",
                          "enhancedtime",
                          "enhancedabu")
  
  enhancedf$enhancedabu <- as.numeric(enhancedf$enhancedabu)
  enhancedf$enhancedarea <- as.numeric(enhancedf$enhancedarea)
  
  enhancedf$enhancedss <- ifelse(enhancedf$enhancedss == "Area/compensation not in local strategy/ no local strategy", "Low", enhancedf$enhancedss)
  enhancedf$enhancedss <- ifelse(enhancedf$enhancedss == "Location ecologically desirable but not in local strategy", "Medium", enhancedf$enhancedss)
  enhancedf$enhancedss <- ifelse(enhancedf$enhancedss == "Formally identified in local strategy", "High", enhancedf$enhancedss)
  
  enhancedf$distinctiveness <- ifelse(enhancedf$distinctiveness == "V.low", "Very Low", enhancedf$distinctiveness)
  enhancedf$distinctiveness <- ifelse(enhancedf$distinctiveness == "V.Low", "Very Low", enhancedf$distinctiveness)
  enhancedf$distinctiveness <- ifelse(enhancedf$distinctiveness == "V.high", "Very High", enhancedf$distinctiveness)
  
  totalenhancedarea <- round(sum(enhancedf$enhancedarea, na.rm = TRUE), 2)
  totalenhancedunits <- round(sum(enhancedf$enhancedabu, na.rm = TRUE), 2)
  
  habitatenhancedata <- list(habitatenhancedata = enhancedf,
                              TotalEnhancedArea = totalenhancedarea,
                              TotalEnhancedUnits = totalenhancedunits)
  
  return(habitatenhancedata)
  
}

#' Off-site habitat gain summary
#'
#' @param metric A BNG metric
#'
#' @returns A data.frame of summary data for off-site biodiversity net gains
#' @export
#'
#' @examples \dontrun{netgainsummary <- offsitehabitatgainsummary(metric)}
offsitehabitatgainsummary <- function(metric){
  
  gainsummary <- openxlsx::read.xlsx(metric,
                                   sheet = "Off-site gain site summary",
                                   cols = 2:13,
                                   rows = 7:107,
                                   startRow = 7,
                                   colNames = FALSE,
                                   skipEmptyRows = TRUE)
  
  colnames(gainsummary) <- c("Gain site reference",
                             "Off-site units baseline",
                             "SRM Off-site units baseline",
                             "Off-site units retained",
                             "SRM Off-site units retained",
                             "Off-site units enhanced",
                             "SRM Off-site units enhanced",
                             "Off-site units created",
                             "SRM Off-site units created",
                             "Off-site unit change per gain site - pre-SRM",
                             "SRM",
                             "Off-site unit change per gain site - post-SRM")
  
  return(gainsummary)
  
}
