
#' New on site habitat baseline function for testing
#'
#' @param metric file path to metric
#'
#' @return list with dataframe of baseline data, total area nd total baselien units
#' @export
#'
#' @examples \dontrun{baselinedata<-newonsitehabitatbaseline(metric)}
newonsitehabitatbaseline <- function(metric){
  
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

# function testing

# res <- microbenchmark::microbenchmark(newbaselinefunction(metric2), pullonsitehabitatbaseline(metric2), times = 10)
# print(res)
# 
# 
# test1 <- newbaselinefunction(metric2) # reads in all rows, despite skip row argument
# test1$habitatbaselinedata
# 
# test2 <- newbaselinefunction(metric)
# test2$habitatbaselinedata


#' New on site habitats retained function for testing
#'
#' @param metric file path to metric
#'
#' @return list with dataframe of retained habitats, total retained area, total retained units
#' @export
#'
#' @examples \dontrun{newonsitehabitatretain(metric)}
newonsitehabitatretain <- function(metric){
  
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

# newonsitehabitatretain(metric2)
# newonsitehabitatretain(metric)

# res <- microbenchmark::microbenchmark(newonsitehabitatretain(metric2), pullonsitehabitatretain(metric2), times = 10)
# print(res)

#' New on site habitat lost function for testing
#'
#' @param metric file path to metric
#'
#' @return list with dataframe of habitats lost, total area lost, and total units lost
#' @export
#'
#' @examples \dontrun{newonsitehabitatloss(metric)}
newonsitehabitatloss <- function(metric){
  
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

# newconsitehabitatloss(metric)                                  
# newconsitehabitatloss(metric2)                                  

#' New on site habitat created function for testing
#'
#' @param metric fiel path to metric
#'
#' @return list with data frame of habitats created, total area of habitats created, and total units created
#' @export
#'
#' @examples \dontrun{newonsitehabitatcreation(metric)}
newonsitehabitatcreation <- function(metric){
  
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

# newonsitehabitatcreation(metric)
# newonsitehabitatcreation(metric2)

