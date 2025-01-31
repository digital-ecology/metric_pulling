#'waterlbupilost
#'
#' @param waterlostdata water loss df
#' @param sectioncounter counter passed to following dynamic area based post intervention sections 
#'
#' @return sectioncounter, passed to future dynamically rendered sections under post intervention
#' @export
#'
#' @examples \dontrun{sectioncounter <- waterlbupilost(waterlossdata$waterlostdata, sectioncounter)}
waterlbupilost<-function(waterlostdata, sectioncounter){
  
  sectioncounter <- sectioncounter + 1
  
  cat(sprintf("## 7.%s Lost Watercourse Biodiversity Units\n\n", sectioncounter))  
  
  subsectioncounter <- 0
  
  #group by habitat type and dplyr::summarize total length lost
  linearhabs <- waterlostdata |>
    dplyr::group_by(habitattype) |>
    dplyr::summarize(totallength = sum(lengthlost, na.rm = TRUE), .groups = "drop")
  
  for (i in 1:nrow(linearhabs)){
      
      subsectioncounter <- subsectioncounter + 1
      
      #render paragraph number
      cat(sprintf("7.%s.%s", sectioncounter, subsectioncounter))
      cat(paste(" A ",(round(linearhabs$totallength[i], 2)), "km length of ", tolower(linearhabs$habitattype[i])," will be lost to the development.\n\n", sep=''))
      
    }
  return(sectioncounter)
}

#' waterlbupiretain
#'
#' @param waterretaindata water retain df
#' @param sectioncounter counter passed to following dynamic area based post intervention sections 
#'
#' @return sectioncounter, passed to future dynamically rendered sections under post intervention
#' @export
#'
#' @examples #' @examples \dontrun{sectioncounter<-waterlbupiretain(waterretaindata$waterretaindata, sectioncounter)}
waterlbupiretain<-function(waterretaindata, sectioncounter){
  
  sectioncounter <- sectioncounter + 1
  
  cat(sprintf("## 7.%s Retained Watercourse Biodiversity Units\n\n", sectioncounter))
  
  subsectioncounter <- 0  
  
  #group by habitat type and dplyr::summarize total length lost
  linearhabs <- waterretaindata |>
    dplyr::group_by(habitattype) |>
    dplyr::summarize(totallength = sum(lengthretained, na.rm = TRUE), .groups = "drop")
  
  for (i in 1:nrow(linearhabs)){
    
    subsectioncounter <- subsectioncounter + 1
    
    #render paragraph number
    cat(sprintf("7.%s.%s", sectioncounter, subsectioncounter))
    cat(paste(" A ",(round(linearhabs$totallength[i], 2)), "km length of ", tolower(linearhabs$habitattype[i])," will be retained.\n\n", sep=''))
  
  }
   return(sectioncounter) 
}

#' waterlbupicreate
#'
#' @param watercreationdata water creation df
#' @param sectioncounter counter passed to following dynamic area based post intervention sections 
#'
#' @return sectioncounter, passed to future dynamically rendered sections under post intervention
#' @export
#'
#' @examples \dontrun{sectioncounter<-waterlbupicreate(watercreationdata$watercreationdata, sectioncounter)}
waterlbupicreate<-function(watercreationdata, sectioncounter){
  
  sectioncounter <- sectioncounter + 1
  
  cat(sprintf("## 7.%s Created Watercourse Biodiversity Units\n\n", sectioncounter))  
  
  subsectioncounter <- 0    
  
  #group by habitat type and dplyr::summarize total length lost
  linearhabs <- watercreationdata |>
    dplyr::group_by(habitattype, createdcondition, createdss, distinctiveness) |>
    dplyr::summarize(totallength = sum(createdlength, na.rm = TRUE))
  
  for (i in 1:nrow(linearhabs)){
    
    subsectioncounter <- subsectioncounter + 1
    
    #render paragraph number
    cat(sprintf("7.%s.%s", sectioncounter, subsectioncounter))
    cat(paste(" A ",(round(linearhabs$totallength[i], 2)), "km length of ", tolower(linearhabs$habitattype[i])," will be created, ", sep=''))
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
#' @param waterenhancementdata waterenhancementdata df
#' @param sectioncounter counter to pass
#'
#' @return counter to pass
#' @export
#'
#' @examples \dontrun{sectioncounter<-waterlbupienhance(waterenhancedata$waterenhancementdata, sectioncounter)}
waterlbupienhance <- function(waterenhancementdata, sectioncounter){
  
  sectioncounter <- sectioncounter + 1
  
  cat(sprintf("## 7.%s Enhanced Watercourse Biodiversity Units\n\n", sectioncounter))  
  
  subsectioncounter <- 0    
  
  #group by habitat type and dplyr::summarize total length lost
  linearhabs <- waterenhancementdata |>
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