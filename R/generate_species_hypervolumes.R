#' Title
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
generate_species_hypervolumes <- function(species_list_selected, rescaled_abiotics){

  hv_list <- c() #a list to be filled with all the species hypervolumes

  for (i in 1:length(species_list_selected)){
    data <- subset(rescaled_abiotics, species==species_list_selected[i])[,2:(length(rescaled_abiotics)-1)]
    print(species_list_selected[i])
    if (log(length(data[[1]]))>(length(rescaled_abiotics)-1)){  #if there are not enough occurrences, an error would appear
      hv_species <- hypervolume(data, method='svm')
      hv_species@Name <- species_list_selected[[i]]
      hv_list<- hypervolume_join(hv_list, hv_species)
    }
    else {
      warning(paste0(species_list_selected[i], "does not have enough values(",length(data[[1]]),") to be studied and has been removed from the list"))
    }
  }
  return(hv_list)
}

#2: In hypervolume(data, method = "svm") :
#Log number of observations (3.83) is less than or equal to the number of dimensions (9).
#You may not have enough data to accurately estimate a hypervolume with this dimensionality.
#Consider reducing the dimensionality of the analysis.
