#' metric_check
#'
#' @param metric metric
#'
#' @return text with list of where data is missing in metric
#' @export
#'
#' @examples \dontrun{
#'a <- metric_check("data-raw/OnSiteHedgeEnhance.xlsx") 
#'b <- metric_check("data-raw/OnSiteHedgeEnhanceMissing.xlsx") # intentionally bad data
#'c <- metric_check("data-raw/OnSiteHedgeEnhance2.xlsx") # metric used in testing qmd}
metric_check <- function(metric) {
  
  # HRem <- checkonsitenet_dataset(metric)
  # TAem <- checkhabsum_dataset(metric)
  # THem <- checkhedgesum_dataset(metric)
  # TWem <- checkwatersum_dataset(metric)
  A1em <- check_A1(metric)
  A2em <- check_A2(metric)
  A3em <- check_A3(metric)
  B1em <- check_B1(metric)
  B2em <- check_B2(metric)
  B3em <- check_B3(metric)
  C1em <- check_C1(metric)
  C2em <- check_C2(metric)
  C3em <- check_C3(metric)
  
  errormessages <- list(
    # "Headline Results" = HRem,
    # "Trading Summary Area Habitats" = TAem,
    # "Trading Summary Hedgerows" = THem,
    # "Trading Summary WaterC's" = TWem,
    "A-1 On-Site Habitat Baseline" = A1em[[2]],
    "A-2 On-Site Habitat Creation" = A2em[[2]],
    "A-3 On-Site Habitat Enhancement" = A3em[[2]],
    "B-1 On-Site Hedge Baseline" = B1em[[2]],
    "B-2 On-Site Hedge Creation" = B2em[[2]],
    "B-3 On-Site Hedge Enhancement" = B3em[[2]],
    "C-1 On-Site WaterC' Baseline" = C1em[[2]],
    "C-2 On-Site WaterC' Creation" = C2em[[2]],
    "C-3 On-Site WaterC' Enhancement" = C3em[[2]]
  )
  
  #as you add the new sections into the checks, you'll get 14 checks, and each will list just the unique columns missing on each sheet - the retain, loss, base, are all the same can just be one function!
  
  #IF there are no error messages, error messages = NULL, so remove all from checks where em = null
  #errormessages <- errormessages[!sapply(errormessages, is.null)]
  
  #initialise empty message storer
  message<-c("Metric Check Results:\n\n")
  
  #if all messages are null, theres no problems
  if(all(sapply(errormessages, is.null))) {
    
    message <- paste(message, "- All sheets filled out correctly. No issues detected.\n", sep = "")
    
  } 
  
  #if any arent null, there is a problem
  if(any(!sapply(errormessages, is.null))){
    
    #remove the null ones 
    errormessages <- errormessages[!sapply(errormessages, is.null)]
  
    #for whatever is left, report issues 
  for (i in 1:length(errormessages)) {
    
      problematicmetric <- names(errormessages)[[i]]
      
      errors <- errormessages[[i]]
    
      message <- paste(message, "- WARNING: Issues detected on", problematicmetric, "sheet:", errors)
      
    }
 }
  
  metricdata <- list(
    # "Headline Results" = HRem,
    # "Trading Summary Area Habitats" = TAem,
    # "Trading Summary Hedgerows" = THem,
    # "Trading Summary WaterC's" = TWem,
    `A-1 On-Site Habitat Baseline` = as.data.frame(A1em[[1]]),
    `A-2 On-Site Habitat Creation` = as.data.frame(A2em[[1]]),
    `A-3 On-Site Habitat Enhancement` = as.data.frame(A3em[[1]]),
    `B-1 On-Site Hedge Baseline` = as.data.frame(B1em[[1]]),
    `B-2 On-Site Hedge Creation` = as.data.frame(B2em[[1]]),
    `B-3 On-Site Hedge Enhancement` = as.data.frame(B3em[[1]]),
    `C-1 On-Site WaterC' Baseline` = as.data.frame(C1em[[1]]),
    `C-2 On-Site WaterC' Creation` = as.data.frame(C2em[[1]]),
    `C-3 On-Site WaterC' Enhancement` = as.data.frame( C3em[[1]])
  )
  
  metriccheckresults<-list(checkresult = message,
                           metricdata = metricdata)
  
  return(metriccheckresults)
  
}  


