#'hedgelbupilost
#'
#' @param hedgelostdata hedge loss df
#' @param sectioncounter counter passed to following dynamic area based post intervention sections 
#'
#' @return sectioncounter, passed to future dynamically rendered sections under post intervention
#' @export
#'
#' @examples \dontrun{sectioncounter <- hedgelbupilost(hedgelossdata$hedgelostdata, sectioncounter)}
hedgelbupilost<-function(hedgelostdata, sectioncounter){
  
  sectioncounter <- sectioncounter + 1
  
  cat(sprintf("## 7.%s Lost Hedgerow Biodiversity Units\n\n", sectioncounter))  
  
  subsectioncounter <- 0
  
  #group by habitat type and dplyr::summarize total length lost
  linearhabs <- hedgelostdata |>
    dplyr::group_by(habitattype) |>
    dplyr::summarize(
      totallength = sum(lengthlost, na.rm = TRUE),
      hedgenums = dplyr::case_when(
        dplyr::n_distinct(hedgenumber) == 1 ~ paste(hedgenumber, collapse = ""),
        dplyr::n_distinct(hedgenumber) == 2 ~ paste(hedgenumber, collapse = " and "),
        dplyr::n_distinct(hedgenumber) > 2 ~ {
          hedges <- unique(hedgenumber)
          paste(paste(hedges[-length(hedges)], collapse = ", "), "and", tail(hedges, 1))
        },
        TRUE ~ ""
      ),
      .groups = "drop"
    )
  
  for (i in 1:nrow(linearhabs)){
    
    subsectioncounter <- subsectioncounter + 1
    
    #render paragraph number
    cat(sprintf("7.%s.%s", sectioncounter, subsectioncounter))
    cat(paste(" A ",(round(linearhabs$totallength[i], 2)), "km length of ", tolower(linearhabs$habitattype[i])," from ", linearhabs$hedgenums[i]," will be lost to the development.\n\n", sep=''))
    
  }
  return(sectioncounter)
}

#' hedgelbupiretain
#'
#' @param hedgeretaindata hedge retain df
#' @param sectioncounter counter passed to following dynamic area based post intervention sections 
#'
#' @return sectioncounter, passed to future dynamically rendered sections under post intervention
#' @export
#'
#' @examples #' @examples \dontrun{sectioncounter<-hedgelbupiretain(hedgeretaindata$hedgeretaindata, sectioncounter)}
hedgelbupiretain<-function(hedgeretaindata, sectioncounter){
  
  sectioncounter <- sectioncounter + 1
  
  cat(sprintf("## 7.%s Retained Hedgerow Biodiversity Units\n\n", sectioncounter))
  
  subsectioncounter <- 0  
  
  #group by habitat type and dplyr::summarize total length lost
  linearhabs <- hedgeretaindata |>
    dplyr::group_by(habitattype) |>
    dplyr::summarize(
      totallength = sum(lengthretained, na.rm = TRUE),
      hedgenums = dplyr::case_when(
        dplyr::n_distinct(hedgenumber) == 1 ~ paste(hedgenumber, collapse = ""),
        dplyr::n_distinct(hedgenumber) == 2 ~ paste(hedgenumber, collapse = " and "),
        dplyr::n_distinct(hedgenumber) > 2 ~ {
          hedges <- unique(hedgenumber)
          paste(paste(hedges[-length(hedges)], collapse = ", "), "and", tail(hedges, 1))
        },
        TRUE ~ ""
      ),
      .groups = "drop"
    )
  
  for (i in 1:nrow(linearhabs)){
    
    subsectioncounter <- subsectioncounter + 1
    
    #render paragraph number
    cat(sprintf("7.%s.%s", sectioncounter, subsectioncounter))
    cat(paste(" A ",(round(linearhabs$totallength[i], 2)), "km length of ", tolower(linearhabs$habitattype[i])," from ", linearhabs$hedgenums[i]," will be retained.\n\n", sep=''))
    
  }
  return(sectioncounter) 
}

#' hedgelbupicreate
#'
#' @param hedgecreationdata hedge creation data
#' @param sectioncounter counter passed to following dynamic area based post intervention sections 
#'
#' @return sectioncounter, passed to future dynamically rendered sections under post intervention
#' @export
#'
#' @examples \dontrun{sectioncounter<-hedgelbupicreate(hedgecreationdata$hedgecreationdata, sectioncounter)}
hedgelbupicreate<-function(hedgecreationdata, sectioncounter){
  
  sectioncounter <- sectioncounter + 1
  
  cat(sprintf("## 7.%s Created Hedgerow Biodiversity Units\n\n", sectioncounter))  
  
  subsectioncounter <- 0    
  
  #group by habitat type and dplyr::summarize total length lost
  linearhabs <- hedgecreationdata |>
    dplyr::group_by(habitattype, createdcondition, createdss, distinctiveness) |>
    dplyr::summarize(totallength = sum(createdlength, na.rm = TRUE),
    hedgenums = dplyr::case_when(
    dplyr::n_distinct(hedgenumber) == 1 ~ paste(hedgenumber, collapse = ""),
    dplyr::n_distinct(hedgenumber) == 2 ~ paste(hedgenumber, collapse = " and "),
    dplyr::n_distinct(hedgenumber) > 2 ~ {
      hedges <- unique(hedgenumber)
      paste(paste(hedges[-length(hedges)], collapse = ", "), "and", tail(hedges, 1))
    },
    TRUE ~ ""
  ),
  .groups = "drop"
  )
  
  for (i in 1:nrow(linearhabs)){
    
    subsectioncounter <- subsectioncounter + 1
    
    #render paragraph number
    cat(sprintf("7.%s.%s", sectioncounter, subsectioncounter))
    cat(paste(" A ",(round(linearhabs$totallength[i], 2)), "km length of ", tolower(linearhabs$habitattype[i])," will be created (", linearhabs$hedgenums[i],"), ", sep=''))
    if (linearhabs$createdcondition[i] %in% c("Condition Assessment N/A", "N/A - Other")) {
      
      cat(paste("a ",tolower(linearhabs$distinctiveness[i])," distinctiveness, ",tolower(linearhabs$createdss[i])," strategic significance habitat. ", sep=''))
      
    } else {
      
      cat(paste("a ",tolower(linearhabs$distinctiveness[i])," distinctiveness, ",tolower(linearhabs$createdss[i])," strategic significance habitat. ", sep=''))
      cat(paste("The target condition for this proposed habitat is '",tolower(linearhabs$createdcondition[i]),"'. ", sep=''))
      
    }
    
    cat(paste("To facilitate the creation of this habitat...\n\n"))
    
  }
  return(sectioncounter)
}

#' linear enhance
#'
#' @param hedgeenhancementdata hedge df
#' @param sectioncounter counter to pass
#'
#' @return counter to pass
#' @export
#'
#' @examples \dontrun{sectioncounter<-hedgelbupienhance(hedgeenhancedata$hedgeenhancementdata, sectioncounter)}
hedgelbupienhance <- function(hedgeenhancementdata, sectioncounter){
  
  sectioncounter <- sectioncounter + 1
  
  cat(sprintf("## 7.%s Enhanced Hedgerow Biodiversity Units\n\n", sectioncounter))  
  
  subsectioncounter <- 0    
  
  #group by habitat type and dplyr::summarize total length lost
  linearhabs <- hedgeenhancementdata |>
    dplyr::group_by(habitattype, basecondition, basehabitattype, enhancedcond, enhancedss, distinctiveness) |>
    dplyr::summarize(totallength = sum(enhancedlength, na.rm = TRUE), .groups = "drop")
  
  for (i in 1:nrow(linearhabs)){
    
    subsectioncounter <- subsectioncounter + 1
    
    #render paragraph number
    cat(sprintf("7.%s.%s", sectioncounter, subsectioncounter))
    
    cat(paste(" A ",(round(linearhabs$totallength[i], 2)), "km length of ", tolower(linearhabs$basecondition[i]), " condition ",tolower(linearhabs$basehabitattype[i])," will be enhanced to ", tolower(linearhabs$enhancedcond[i]), " condition ",tolower(linearhabs$habitattype[i]),",", sep=''))
    cat(paste(" a ",tolower(linearhabs$distinctiveness[i])," distinctiveness, ",tolower(linearhabs$enhancedss[i])," strategic significance habitat. ", sep=''))
    cat(paste("To facilitate such an enhancement, the habitat should be...\n\n"))
    
  }
  
  return(sectioncounter)
  
}