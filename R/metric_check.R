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
  checks <- list(
    "A-1 On-Site Habitat Baseline (columns 5,6,8,9,11,13,17)" = pullonsitehabitatbaseline(metric),
    "A-1 On-Site Habitat Baseline (columns 5,6,19,21)" = pullonsitehabitatretain(metric),
    "A-1 On-Site Habitat Baseline (columns 5,6,23,24)" = pullonsitehabitatloss(metric),
    "A-2 On-Site Habitat Creation (columns 4,5,7,8,10,12,19,25)" = pullonsitehabitatcreation(metric),
    "A-3 On-Site Habitat Enhancement (columns 6,17,18,22,23,25,27,30,40)" = pullonsitehabitatenhancement(metric),
    "Headline Results (rows 8,9,10,47,48,49,51,52,53,55,61,62,63)" = pullonsitenetdata(metric),
    "Trading Summary Area Habitats" = pullonsitehabitatsumdata(metric),
    "Trading Summary Hedgerows" = pullonsitehedgesumdata(metric),
    "Trading Summary WaterC's" = pullonsitewatersumdata(metric),
    "B-1 On-Site Hedge Baseline (columns 3,5,8,10,14,17,19)" = pullonsitehedgebaseline(metric),
    "B-1 On-Site Hedge Baseline (columns 3,4,16,18)" = pullonsitehedgeretain(metric),
    "B-1 On-Site Hedge Baseline (columns 3,4,20,21)" = pullonsitehedgeloss(metric),
    "B-2 On-Site Hedge Creation (columns 3,4,5,6,8,10,23) " = pullonsitehedgecreation(metric),
    "B-3 On-Site Hedge Enhancement (columns 2,3,7,16,17,19,21,34)" = pullonsitehedgeenhancement(metric),
    "C-1 On-Site WaterC' Baseline (columns 4,5,6,8,10,24)" = pullonsitewaterbaseline(metric),
    "C-1 On-Site WaterC' Baseline (columns 4,23)" = pullonsitewaterretain(metric),
    "C-1 On-Site WaterC' Baseline (columns 4,25,26)" = pullonsitewaterloss(metric),
    "C-2 On-Site WaterC' Creation (columns 3,4,5,7,9,26,29)" = pullonsitewatercreation(metric),
    "C-3 On-Site WaterC' Enhancement (columns 3,7,14,17,18,20,22,39)" = pullonsitewaterenhancement(metric)
  )
  
  # outputs to character to allow for proper matching
  checks_as_character <- vapply(checks, function(x) {
    if (is.character(x) && length(x) == 1) {
      return(x)
    } else {
      return("OK")  # default for non-matching values - doesnt return out though
    }
  }, FUN.VALUE = character(1))
  
  # ceheck functions returning the specified check message
  issues <- names(checks_as_character)[checks_as_character == "Please check metric is filled in appropriately before continuing"]
  
  # format the output message
  if (length(issues) > 0) {
    issue_list <- paste("- ", issues, collapse = " ")
    message <- paste("Please check the following metrics are filled in correctly before running the app: ", issue_list)
  } else {
    message <- "All metrics are correctly filled in. No issues detected."
  }
  return(message)
  print(message)
}  


