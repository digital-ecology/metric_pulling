

#' On-site hedge baseline data
#'
#' @param metric A BNG metric
#'
#' @return A list with a data.frame of summary data, and numerics for total length
#' and total linear baseline units
#' @export
#'
#' @examples \dontrun{}
pullonsitehedgebaseline <- function(metric){
  
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

#' On-site hedgerow habitat retained
#'
#' @param metric A BNG metric
#'
#' @return A list with a summary data.frame, and numerics for total length and
#' units of hedgerow retained.
#' @export
#'
#' @examples \dontrun{}
pullonsitehedgeretain <- function(metric){
  
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



#' Hedge habitat lost data summary
#'
#' @param metric A BNG metric
#'
#' @return A list witha  data.frame summary of hedgerows lost, and numerics for 
#' total lenght and linear biodiversity units lost.
#' @export
#'
#' @examples \dontrun{}
pullonsitehedgeloss <- function(metric){
  
  hedgeloss <- openxlsx::read.xlsx(metric,
                                   sheet = "B-1 On-Site Hedge Baseline",
                                   cols = c(3,4,18,20),
                                   rows = 10:257,
                                   startRow = 10,
                                   colNames = FALSE,
                                   skipEmptyRows = TRUE)
  
  hedgeloss <- na.omit(hedgeloss)
  
  colnames(hedgeloss) <- c("hedgenumber",
                           "habitattype",
                           "lengthlost",
                           "unitslost")
  
  hedgeloss$lengthlost <- as.numeric(hedgeloss$lengthlost)
  hedgeloss$unitslost <- as.numeric(hedgeloss$unitslost)
  
  totallengthlost <- round(sum(hedgeloss$lengthlost, na.rm = TRUE), 3) # may not need these two
  totallbulost <- round(sum(hedgeloss$unitslost, na.rm = TRUE), 3)
  
  # add in catch for if no hedge lost
  #if youre left with nothing, insert placeholder
  # if (nrow(hedgelostdata) == 0) {
  #   hedgelostdata <- data.frame(habitattype = "No Hedgerows Lost")
  # }
  # 
  # } else {
  # hedgelostdata <- data.frame(hedgehabitattype)
  # }
  
  hedgelossdata<-list(hedgelostdata = hedgeloss,
                          totallength = totallengthlost,
                          totalunits = totallbulost)
  
  return(hedgelossdata)
  
}


pullonsitehedgecreation <- function(metric){
  
  hedgecreate <- openxlsx::read.xlsx(metric,
                                     sheet = "B-2 On-Site Hedge Creation")
  
}