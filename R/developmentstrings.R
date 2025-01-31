#' dev string
#'
#' @param losthabitattype lost habs
#' @param losthedgetype lost hedges
#' @param lostwatertype lost waters
#'
#' @return string for development
#' @export
#'
#' @examples \dontrun{developmentstring<-developmentstring(habitatlossdata$habitatlostdata$habitattype, hedgelossdata$hedgelostdata$habitattype, waterlossdata$waterlostdata$habitattype)}
developmentstring<-function(losthabitattype, losthedgetype, lostwatertype){
  
  #concatenate into one df, losthabitattype
  lostlist<-list(losthabitattype, losthedgetype, lostwatertype)
  
  #remove where No Habitats Lost or No Hedges Lost or No Watercourses Lost
  lostlist <- lostlist[!lostlist %in% c("No Habitats Lost", "No Hedges Lost", "No Watercourses Lost")]
  
  if (length(lostlist) != 0) {
  
  losthabitattype<-unlist(lostlist)
  uniquelost <- unique(losthabitattype)
  uniquelost <- as.data.frame(uniquelost)
  numlost <- nrow(uniquelost)
  
  if (numlost > 1) {
    if (numlost > 2) {
      developmentstring <- paste(paste(head(uniquelost$uniquelost, -2), collapse = ", "),
                                 paste(tail(uniquelost$uniquelost, 2), collapse = " and "),
                                 sep = ", ")
    } else {
      developmentstring <- paste(uniquelost$uniquelost[1], uniquelost$uniquelost[2], sep = " and ")
    }
  } else {
    developmentstring <- uniquelost$uniquelost
  }} else {
    
    developmentstring <- "nothing"
  
    }
  
  return(tolower(developmentstring))
  
}
