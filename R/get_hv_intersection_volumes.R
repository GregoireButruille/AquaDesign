#' Title Generate a dataframe with all the combination's intersections volumes
#'
#' @param hv_list A list of hypervolumes
#'
#' @return
#' @export
#'
#' @examples
get_hv_intersection_volumes <- function(hv_list,nb = NA){

  if (is.na(nb)){
    #ask for the max number of species in combinations
    nb_combi <- dlg_list(title = "Chose the max number of species in combinations", c(2:(length(hv_list@HVList))))$res
    nb_combi <- as.numeric(nb_combi)
  } else{
    nb_combi <-nb
  }

  #get species_list from hv_list
  species_list <- c()
  for (i in 1:length(hv_list@HVList)){
    species_list <- c(species_list, hv_list[[i]]@Name)
  }

  #get all the combinations possible among the species of the list
  list_combi <- do.call("c", lapply(1:nb_combi, function(i) combn(species_list, i, FUN = list)))

  #remove the single species from the list (which are the first elements)
  list_combi <- list_combi[-(1:length(species_list))]

  #inform about the number of combinations
  print(paste0("combinations to calculate :" , as.character(length(list_combi))))

  combi_df <- data.frame(matrix(ncol = nb_combi, nrow = 0))
  #convert the combinations list into a dataframe
  for ( i in 1:length(list_combi)){
    vect <- list_combi[[i]]
    length(vect) <- nb_combi+1
    combi_df <- rbind(combi_df, vect)
  }

  #replace NAs by "none" to be able to make comparisons later
  combi_df[is.na(combi_df)] <- "None"

  #for each combination
  for (i in 1:length(combi_df[,1])){

    #Initialize and fill a list of indexes corresponding of the Hypervolumes to compare  in combination number i
    ind <- c()
    hv_list_test <- new("HypervolumeList")

    for (j in 1:length(species_list)){
      for (q in 1:length(combi_df[1,])){

        if (combi_df[[q]][i] == hv_list[[j]]@Name){
          ind <- c(ind, j)
        }
      }
    }

    #make a list with the hypervolumes to compare and run the comparison function, add the volume to the dataframe
    hv_list_test <-  hv_list[[ind]]
    intersection <- hypervolume_set_n_intersection(hv_list_test)
    combi_df[[nb_combi+1]][i] <- intersection@Volume
  }

  #rescale the volumes between 0 and 1
  combi_df[nb_combi+1] <- as.numeric(combi_df[[nb_combi+1]])
  rescaled_combi_df <-cbind(combi_df[1:nb_combi], apply(combi_df[nb_combi+1], MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))))

  return(rescaled_combi_df)
}

