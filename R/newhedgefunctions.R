

newonsitehedgebaseline <- function(metric){
  
  hedgebase <- openxlsx::read.xlsx(metric,
                                   sheet = "B-1 On-Site Hedge Baseline",
                                   cols = c(3,4,5,8,10,14),
                                   rows = 10:257,
                                   startRow = 10,
                                   colNames = FALSE,
                                   skipEmptyRows = TRUE)
  
  hedgebase <- na.omit(hedgebase)
  
  colnames(hedgebase) <- c("baseid",
                           "habitattype",
                           "baselinelength",
                           "baselinecondition",
                           "baseliness",
                           "baselinelbu")
  
  hedgebase$baseliness <- ifelse(hedgebase$baseliness == "Area/compensation not in local strategy/ no local strategy", "Low", hedgebase$baseliness)
  hedgebase$baseliness <- ifelse(hedgebase$baseliness == "Location ecologically desirable but not in local strategy", "Medium",hedgebase$baseliness)
  hedgebase$baseliness <- ifelse(hedgebase$baseliness == "Formally identified in local strategy", "High",hedgebase$baseliness)
 
  hedgebase$baselinelength <- as.numeric(hedgebase$baselinelength)
  hedgebase$baselinelbu <- as.numeric(hedgebase$baselinelbu)
 
  totallength <- sum(hedgebase$baselinelength, na.rm = TRUE)
  totallbu <- sum(hedgebase$baselinelbu, na.rm = TRUE)
 
  hedgebaselinedata<-list(hedgebaselinedata = hedgebase,
                         totallength = totallength,
                         totalunits = totallbu)
 
  return(hedgebaselinedata)
  
}

# res <- newonsitehedgebaseline(metric)
# res$hedgebaselinedata
# res$totallength
# res$totalunits

# res <- microbenchmark::microbenchmark(newonsitehedgebaseline(metric), pullonsitehedgebaseline(metric), times = 10)
# print(res)

newonsitehedgeretain <- function(metric){
  
  hedgeretain <- openxlsx::read.xlsx(metric,
                                     sheet = "B-1 On-Site Hedge Baseline",
                                     cols = c(3, 4, 16, 18), # doesn't return an empty col
                                     rows = 10:257,
                                     startRow = 10,
                                     colNames = FALSE,
                                     skipEmptyRows = TRUE)
  
  hedgeretain <- na.omit(hedgeretain)
  
  colnames(hedgeretain) <- c("hedgenumber",
                            "habitattype",
                            "lengthretained",
                            "lburetained")
  
  
  
}
