#' Title Generate a list of hypervolume from a list of species
#'
#' @param species_list_selected List of the selected species
#' @param rescaled_abiotics Selected and rescaled abiotics pca axis in a data frame
#' @param nb_axis Number of axis retained in the pca
#'
#'@import hypervolume
#' @return
#' @export
#'
#' @examples
generate_species_hv <- function(species_list, rescaled_abiotics){

  hv_list <- c() #a list to be filled with all the species hypervolumes

  for (i in 1:length(species_list)){
    data <- subset(rescaled_abiotics, species==species_list[i])[,2:length(rescaled_abiotics)]
    progression=paste0("Hypervolumes generation... ",i,"/",length(species_list)," : ",species_list[i])
    print(progression)
    if (log(length(data[[1]]))>(length(rescaled_abiotics)-1)){  #if there are not enough occurrences for some species, a warning appear
      hv_species <- hypervolume(data, method='svm') #generate hypervolume with single vector method
      hv_species@Name <- species_list[[i]] #set hypervolume name
      hv_list<- hypervolume_join(hv_list, hv_species) #add the hypervolume to the list
    }
    else {
      warning(paste0(species_list[i], " does not have enough values( ",length(data[[1]]), ") to be studied and has been removed from the list, please remove them from species_list and rerun it"))
    }
  }
  return(hv_list)
}


