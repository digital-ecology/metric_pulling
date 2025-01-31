#' abusummary
#'
#' @param habitatbaselinedata baselinehabdata
#' @param sectioncounter counter to render numbers
#'
#' @return printed text
#' @export
#'
#' @examples \dontrun{abusummary(habitatbaselinedata, sectioncounter)}
abusummary<-function(habitatbaselinedata, sectioncounter){
  
  for (i in 1:nrow(habitatbaselinedata)){
    
    #render title with unique habitat name
    cat(sprintf("### %s\n\n", habitatbaselinedata$habitattype[i]))
    
    sectioncounter <- sectioncounter + 1
    
    #render paragraph number
    cat(sprintf("5.2.%s", sectioncounter))
    cat(paste(" The site consists of a ",(round(habitatbaselinedata$baselinearea[i], 2)), "ha area of ", tolower(habitatbaselinedata$habitattype[i]),", a ", tolower(habitatbaselinedata$distinctiveness[i]), " distinctiveness, ",tolower(habitatbaselinedata$baseliness[i])," strategic significance habitat.\n\n", sep=''))
    
    sectioncounter <- sectioncounter + 1
    
    if (habitatbaselinedata$baselinecondition[i] %in% c("Condition Assessment N/A", "N/A - Other")) {
      
      #for habs where not mod / low / poor / good, insert this string
      habitatbaselinedata$baselinecondition[i] <- "an unassessable"
      
      cat(sprintf("5.2.%s", sectioncounter)) 
      cat(paste(" This habitat is in ", tolower(habitatbaselinedata$baselinecondition[i]), " condition.\n\n", sep=''))
      
    } else {
  
      cat(sprintf("5.2.%s", sectioncounter)) 
      cat(paste(" This habitat is in ", tolower(habitatbaselinedata$baselinecondition[i]), " condition. It meets this condition because of x, y, and z.\n\n", sep=''))
    
    #can code in insertion of pictures once know structure
    
    }}
  
}