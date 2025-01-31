#' water lbu summary
#'
#' @param waterbaselinedata data for baseline watercourses
#' @param sectioncounter counter for dynamic rendering
#'
#' @return rendered section
#' @export
#'
#' @examples \dontrun{waterlbusummary(waterbaselinedata, sectioncounter)}
waterlbusummary<-function(waterbaselinedata, sectioncounter){
  # as.numeric(waterbaselinedata$waterbaselinedata$ 
    
  for (i in 1:nrow(waterbaselinedata)){
    
    #render title with unique habitat name
    cat(sprintf("**%s**\n\n", waterbaselinedata$habitattype[i]))
    
    sectioncounter <- sectioncounter + 1
    
    #render paragraph number
    cat(sprintf("5.2.%s", sectioncounter))
    cat(paste(" The watercourse consists of a ",(round(waterbaselinedata$baselinelength[i], 2)), "km length of ", tolower(waterbaselinedata$habitattype[i]),", a ", tolower(waterbaselinedata$distinctiveness[i]), " distinctiveness, ",tolower(waterbaselinedata$baseliness[i])," strategic significance habitat.\n\n", sep=''))
    
    sectioncounter <- sectioncounter + 1
    
    if (waterbaselinedata$baselinecondition[i] %in% c("Condition Assessment N/A", "N/A - Other")) {
      
      #for habs where not mod / low / poor / good, insert this string
      waterbaselinedata$baselinecondition[i] <- "an unassessable"
      
      cat(sprintf("5.2.%s", sectioncounter)) 
      cat(paste(" This watercourse is in ", tolower(waterbaselinedata$baselinecondition[i]), " condition.\n\n", sep=''))
      
    } else {
    
    cat(sprintf("5.2.%s", sectioncounter)) 
    cat(paste(" This watercourse is in ", tolower(waterbaselinedata$baselinecondition[i]), " condition. It meets this condition because of x, y, and z.\n\n", sep=''))
    
    #can code in insertion of pictures once know structure
    # makesure waterbaseline data is numeric 
  }}
  
}