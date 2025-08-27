
#' On site habitat baseline
#'
#' @param metric file path to metric
#'
#' @return list with data.frame of baseline data, total area nd total baseline units
#' @export
#'
#' @examples {pullonsitehabitatbaseline(system.file("extdata", "OnSiteBoth.xlsx", package = "metricpulling"))}
pullonsitehabitatbaseline <- function(metric){
  
  basedf <- openxlsx::read.xlsx(metric,
                                sheet = "A-1 On-Site Habitat Baseline",
                                cols = c(5, 6, 8, 9, 11, 13, 17),
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


#' On site habitats retained
#'
#' @param metric file path to metric
#'
#' @return list with data.frame of retained habitats, total retained area, total retained units
#' @export
#'
#' @examples \dontrun{pullonsitehabitatretain(metric)}
pullonsitehabitatretain <- function(metric){
  
  retaindf <- openxlsx::read.xlsx(metric,
                                sheet = "A-1 On-Site Habitat Baseline",
                                cols = c(5, 6, 19, 21),
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


#' On site habitat lost function
#'
#' @param metric file path to metric
#'
#' @return list with data.frame of habitats lost, total area lost, and total units lost
#' @export
#'
#' @examples \dontrun{pullonsitehabitatloss(metric)}
pullonsitehabitatloss <- function(metric){
  
  lostdf <- openxlsx::read.xlsx(metric,
                                  sheet = "A-1 On-Site Habitat Baseline",
                                  cols = c(5, 6, 23, 24),
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

                               
#' On site habitat created function for testing
#'
#' @param metric file path to metric
#'
#' @return list with data.frame of habitats created, total area of habitats created, and total units created
#' @export
#'
#' @examples \dontrun{pullonsitehabitatcreation(metric)}
pullonsitehabitatcreation <- function(metric){
  
  createdf <- openxlsx::read.xlsx(metric,
                                sheet = "A-2 On-Site Habitat Creation",
                                cols = c(4, 5, 7, 8, 10, 12, 19, 25),
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
  createdf$createdarea <- as.numeric(createdf$createdabu)
  
  createdf$createdss <- ifelse(createdf$createdss == "Area/compensation not in local strategy/ no local strategy", "Low", createdf$createdss)
  createdf$createdss <- ifelse(createdf$createdss == "Location ecologically desirable but not in local strategy", "Medium", createdf$createdss)
  createdf$createdss <- ifelse(createdf$createdss == "Formally identified in local strategy", "High", createdf$createdss)
  
  createdf$distinctiveness <- ifelse(createdf$distinctiveness == "V.low", "Very Low", createdf$distinctiveness)
  createdf$distinctiveness <- ifelse(createdf$distinctiveness == "V.Low", "Very Low", createdf$distinctiveness)
  createdf$distinctiveness <- ifelse(createdf$distinctiveness == "V.high", "Very High", createdf$distinctiveness)
  
  totalcreationarea <- sum(createdf$createdarea, na.rm = TRUE)
  totalcreationunits <- sum(createdf$createdabu, na.rm = TRUE)
  
  habitatcreationdata <- list(habitatcreationdata = createdf,
                              TotalCreationArea = totalcreationarea,
                              TotalCreationUnits = totalcreationunits)
  
  return(habitatcreationdata)
  
}


#' On-site habitat enhancement summary data
#'
#' @param metric A BNG metric
#'
#' @returns A list with a summary data.frame, an numeric for total habitat area 
#' enhanced and a numeric for total BNG units enhanced
#' @export
#'
#' @examples \dontrun{enhancedata <- pullonsitehabitatenhance(metric)}
pullonsitehabitatenhance <- function(metric){
  
  enhancedf <- openxlsx::read.xlsx(metric,
                                   sheet = "A-3 On-Site Habitat Enhancement",
                                   cols = c(17,18,22,23,25,27,34,40),
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


