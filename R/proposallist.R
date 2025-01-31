#' proposed list, to be used in X section
#'
#' @param enhancedata produced by pulloffsiteenhancement
#' @param creationdata produced by pulloffsitecreation
#'
#' @return string of proposed habitat types to be used in proposal section
#' @export
#'
#' @examples \dontrun{proposedlist<-feas_proposedlist(enhancedata, creationdata)}
feas_proposedlist<-function(enhancedata, creationdata){

  proposed_creation_list <- list()

  #generate strings and store in the list
  for (i in 1:nrow(creationdata$BroadCreation)) {
    proposed_creation_list[[i]] <- paste(creationdata$BroadCreation[i, 1], " - ", creationdata$CreationHabitatType[i, 1], "\n")
  }

  #create proposed enhancement list
  proposed_enhancement_list <- list()

  for (i in 1:nrow(enhancedata$BroadEnhance)){
    proposed_enhancement_list[[i]] <- paste(enhancedata$BroadEnhance[i, 1], " - ", enhancedata$EnhanceHabitatType[i, 1], "\n")
  }

  #merge lists
  proposedlist<-c(proposed_creation_list, proposed_enhancement_list)

  #get unique
  proposedlist <- unique(proposedlist)
  #remove where unique_proposed is either No Habitats Created - No Habitats Created or No Habitats Enhanced - No Habitats Enhanced
  proposedlist <- proposedlist[proposedlist != "No Habitats Created  -  No Habitats Created \n"]
  proposedlist <- proposedlist[proposedlist != "No Habitats Enhanced  -  No Habitats Enhanced \n"]

  return(proposedlist)
}
