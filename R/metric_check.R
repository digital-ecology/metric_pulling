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
    "pullonsitehabitatbaseline" = pullonsitehabitatbaseline(metric),
    "pullonsitehabitatretain" = pullonsitehabitatretain(metric),
    "pullonsitehabitatloss" = pullonsitehabitatloss(metric),
    "pullonsitehabitatcreation" = pullonsitehabitatcreation(metric),
    "pullonsitehabitatenhancement" = pullonsitehabitatenhancement(metric),
    "pullonsitenetdata" = pullonsitenetdata(metric),
    "pullonsitehabitatsumdata" = pullonsitehabitatsumdata(metric),
    "pullonsitehedgesumdata" = pullonsitehedgesumdata(metric),
    "pullonsitewatersumdata" = pullonsitewatersumdata(metric),
    "pullonsitehedgebaseline" = pullonsitehedgebaseline(metric),
    "pullonsitehedgeretain" = pullonsitehedgeretain(metric),
    "pullonsitehedgeloss" = pullonsitehedgeloss(metric),
    "pullonsitehedgecreation" = pullonsitehedgecreation(metric),
    # "pullonsitehedgeenhancement" = pullonsitehedgeenhancement(metric),
    "pullonsitewaterbaseline" = pullonsitewaterbaseline(metric),
    "pullonsitewaterretain" = pullonsitewaterretain(metric),
    "pullonsitewaterloss" = pullonsitewaterloss(metric),
    "pullonsitewatercreation" = pullonsitewatercreation(metric),
    "pullonsitewaterenhancement" = pullonsitewaterenhancement(metric)
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
    issue_list <- paste("- ", issues, collapse = "\n")
    message <- paste("Please check the following metrics are filled in correctly before running the app:\n", issue_list)
  } else {
    message <- "All metrics are correctly filled in. No issues detected."
  }
  return(message)
  print(message)
}  


