#' baseline list, the list of baseline habitats on site
#'
#' @param baselinedata output of pulloffsitebaseline
#'
#' @return list of strings containing the baseline habitats on site
#' @export
#'
#' @examples \dontrun{baselinehabitatlist<-intro_habbaselinelist(baselinedata)}
intro_habbaselinelist<-function(baselinedata){

  #make list for bullet points listing baseline habitats on site - NOT slimmed down by unique yet
  baseline_list <- list()

  #generate strings and store in the list
  for (i in 1:nrow(baselinedata)) {
    baseline_list[[i]] <- paste(baselinedata[i, 1], " - ", baselinedata[i, 2], "\n")
  }

  #get unique
  baseline_list<-unique(baseline_list)

  return(BaselineList = baseline_list)
}

#' baseline list, the list of baseline habitats on site
#'
#' @param baselinedata output of pulloffsitebaseline
#'
#' @return list of strings containing the baseline habitats on site
#' @export
#'
#' @examples \dontrun{baselinewaterlist<-intro_linearbaselinelist(baselinedata)}
intro_linearbaselinelist<-function(baselinedata){
  
  #make list for bullet points listing baseline habitats on site - NOT slimmed down by unique yet
  baseline_list <- list()
  
  #generate strings and store in the list
  for (i in 1:nrow(baselinedata)) {
    baseline_list[[i]] <- paste(baselinedata[i, 1], "\n")
  }
  
  #get unique
  baseline_list<-unique(baseline_list)
  
  return(BaselineList = baseline_list)
}

#' get lpa string for introduction section
#'
#' @param lpa_names the dataframe pulled by getlpa, containing the name(s) of
#' the LPA district the site is in.
#'
#' @return a string containing the LPAs the site is in.
#' @export
#'
#' @examples \dontrun{lpastring<-lpastring(lpa_names)}
lpastring<-function(lpa_names){

  if (nrow(lpa_names) > 1) {
    #concatenate if longer than one
    lpa_names_str <- paste(lpa_names$name, collapse = ", and ")
  } else {
    lpa_names_str <- paste(lpa_names$name)
  }

  return(lpa_names_str)

}
