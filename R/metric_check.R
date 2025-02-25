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
  
  A1em<-clean_onsitehab_baseline(metric)
  
  checks <- list(
    "A-1 On-Site Habitat Baseline" = A1em
    # "A-1 On-Site Habitat Baseline (columns 5,6,19,21)" = clean_onsitehab_retain(metric),
    # "A-1 On-Site Habitat Baseline (columns 5,6,23,24)" = clean_onsitehab_loss(metric),
    # "A-2 On-Site Habitat Creation (columns 4,5,7,8,10,12,19,25)" = clean_onsitehab_creation(metric),
    # "A-3 On-Site Habitat Enhancement (columns 6,17,18,22,23,25,27,30,40)" = clean_onsitehab_enhancement(metric),
    # "Headline Results (rows 8,9,10,47,48,49,51,52,53,55,61,62,63)" = clean_onsitenet_dataset(metric),
    # "Trading Summary Area Habitats" = clean_habsum_dataset(metric),
    # "Trading Summary Hedgerows" = clean_hedgesum_dataset(metric),
    # "Trading Summary WaterC's" = clean_watersum_dataset(metric),
    # "B-1 On-Site Hedge Baseline (columns 3,5,8,10,14,17,19)" = clean_onsitehedge_baseline(metric),
    # "B-1 On-Site Hedge Baseline (columns 3,4,16,18)" = clean_onsitehedge_retain(metric),
    # "B-1 On-Site Hedge Baseline (columns 3,4,20,21)" = clean_onsitehedge_loss(metric),
    # "B-2 On-Site Hedge Creation (columns 3,4,5,6,8,10,23) " = clean_onsitehedge_creation(metric),
    # "B-3 On-Site Hedge Enhancement (columns 2,3,7,16,17,19,21,34)" = clean_onsitehedge_enhancement(metric),
    # "C-1 On-Site WaterC' Baseline (columns 4,5,6,8,10,24)" = clean_C1_dataset(metric),
    # "C-1 On-Site WaterC' Baseline (columns 4,23)" = clean_C1_dataset(metric),
    # "C-1 On-Site WaterC' Baseline (columns 4,25,26)" = clean_C1_dataset(metric),
    # "C-2 On-Site WaterC' Creation (columns 3,4,5,7,9,26,29)" = clean_C2_dataset(metric),
    # "C-3 On-Site WaterC' Enhancement (columns 3,7,14,17,18,20,22,39)" = clean_C3_dataset(metric)
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
   
   return(message)
   
 }
    
    
    
  
  
  
  # # Check functions returning the specified check message
  # issues <- names(checks_as_character)[checks_as_character == "Please check metric is filled in appropriately before continuing"]
  # 
  # # Format the output message
  # message <- if (length(issues) > 0) {
  #   issue_list <- paste("- ", issues, collapse = "\n")  # Each issue on a new line
  #   paste("Please check the following metrics are filled in correctly before running the app:\n", issue_list)
  # } else {
  #   "All metrics are correctly filled in. No issues detected."
  # }
  # 
  # # Print and return message
  # #print(message)
  # return(message)
  
}  


