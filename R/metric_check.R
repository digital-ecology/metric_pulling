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
  
  HRem <- clean_onsitenet_dataset(metric)
  TAem <- clean_habsum_dataset(metric)
  THem <- clean_hedgesum_dataset(metric)
  TWem <- clean_watersum_dataset(metric)
  A1em <- clean_onsitehab_baseline(metric)
  A2em <- clean_onsitehab_creation(metric)
  A3em <- clean_onsitehab_enhancement(metric)
  B1em <- clean_onsitehedge_baseline(metric)
  B2em <- clean_onsitehedge_creation(metric)
  B3em <- clean_onsitehedge_enhancement(metric)
  C1em <- clean_c1_dataset(metric)
  C2em <- clean_c2_dataset(metric)
  C3em <- clean_c3_dataset(metric)
  
  checks <- list(
    "Headline Results" = HRem,
    "Trading Summary Area Habitats" = TAem,
    "Trading Summary Hedgerows" = THem,
    "Trading Summary WaterC's" = TWem,
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


