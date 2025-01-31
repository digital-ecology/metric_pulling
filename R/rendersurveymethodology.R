#' render survey dynamically
#'
#' @param survey survey methodology to input
#' @param sectionCounter counter for title number
#'
#' @return section counter, to be passed to next dynamically rendered subsection
#' @export
#'
#' @examples \dontrun{sectioncounter<-rendersurveymethodology("Grass", sectionCounter)}
rendersurveymethodology <- function(survey, sectionCounter){
  
    filename <- paste0(survey,".qmd")
    
    #hardcode the section titles
    sectionTitle <- switch(
      survey,
      "Grass" = "Grassland Survey Methodology",
      "Hedge" = "Hedgerow Survey Methodology",
      "Water" = "Morph River Assessment",
      "Limits" = "Limitations and Assumptions",
      paste("Unknown Section", survey)
    )
    
    if (file.exists(filename)) {
      
      #read file content
      fileContent <- readLines(filename)
      
      #update section counter to render section number dynamically
      sectionCounter <- sectionCounter + 1
      
      #render title with unique LPA name
      cat(sprintf("## 3.%d %s\n\n", sectionCounter, sectionTitle))
      
      #insert dynamic section counter
      processedContent <- gsub("sectionNumber", sectionCounter, paste(fileContent, collapse = "\n"))
      
      #paste file content
      cat(processedContent, "\n\n")
      
    } else {
      
      #update section counter to render section number dynamically
      sectionCounter <- sectionCounter + 1
      
      #render title and warn of missing template for now
      cat(sprintf("### 3.%d %s\n\n", sectionCounter, sectionTitle))
      cat(sprintf("No %s template found.", sectionTitle)) #could have interaction in app where if no policy, prints that none, but ones someones checked, can change file lines to 'No Content', and make catch so doesnt render
    }
    
  return(sectionCounter)
    
}
