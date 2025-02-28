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
  
  checks <- list(
    # "Headline Results" = HRem,
    # "Trading Summary Area Habitats" = TAem,
    # "Trading Summary Hedgerows" = THem,
    # "Trading Summary WaterC's" = TWem,
    "A-1 On-Site Habitat Baseline" = A1em,
    "A-2 On-Site Habitat Creation" = A2em,
    "A-3 On-Site Habitat Enhancement" = A3em,
    "B-1 On-Site Hedge Baseline" = B1em,
    "B-2 On-Site Hedge Creation" = B2em,
    "B-3 On-Site Hedge Enhancement" = B3em,
    "C-1 On-Site WaterC' Baseline" = C1em,
    "C-2 On-Site WaterC' Creation" = C2em,
    "C-3 On-Site WaterC' Enhancement" = C3em
  )
  
  #as you add the new sections into the checks, you'll get 14 checks, and each will list just the unique columns missing on each sheet - the retain, loss, base, are all the same can just be one function!
  
  #IF there are no error messages, error messages = NULL, so remove all from checks where em = null
  checks <- checks[!sapply(checks, is.null)]
  
  #initialise empty message storer
  message<-c("Metric Check Results:\n\n")
  
  #If checks is then a list of length 0, you know there's no errors. if it's not 0, you know you have issue
 if (length(checks) == 0 ) {message <- paste(message, "All metrics are correctly filled in. No issues detected.")
 
 } else {
    
    for (i in 1:length(checks)) {
      
      problematicmetric <- names(checks)[i]
      
      errors <- checks[[i]]
    
      message <- paste(message, "Issues detected on", problematicmetric, "sheet:", errors)
      
    }
   
   
 }
  return(message)
  
}  


