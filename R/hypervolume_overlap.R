#' Title Generate a dataframe with all the combination's intersections volumes
#'
#' @param hv_list A list of hypervolumes
#'
#' @return
#' @export
#'
#' @examples
get_hv_intersection_volumes <- function(hv_list){

  nb_combi <- dlg_list(title = "Chose the max number of species in combinations", c(2:(length(hv_list@HVList))))$res
  nb_combi <- as.numeric(nb_combi)

  #get species_list from hv_list
  species_list <- c()
  for (i in 1:length(hv_list@HVList)){
    species_list <- c(species_list, hv_list[[i]]@Name)
  }

  #get all the combinations possible among the species of the list
  list_combi <- do.call("c", lapply(1:nb_combi, function(i) combn(species_list, i, FUN = list)))
  list_combi <- list_combi[-(1:length(species_list))] #remove the single species

  print(paste0("combinations to calculate :" , as.character(length(list_combi))))

  combi_df <- data.frame(matrix(ncol = nb_combi, nrow = 0))

  #convert the list of combinations in a dataframe to fill
  for ( i in 1:length(list_combi)){
    vect <- list_combi[[i]]
    length(vect) <- nb_combi+1
    combi_df <- rbind(combi_df, vect)
  }

  #convert na into character (to avoid error in if() below)
  combi_df[is.na(combi_df)] <- "None"

  #generate overlap and extract volume for each combination, filling the dataframe
  for (i in 1:length(combi_df[,1])){
    ind <- c()
    hv_list_test <- new("HypervolumeList")

    for (j in 1:length(species_list)){
      for (q in 1:length(combi_df[1,])){

        if (combi_df[[q]][i] == hv_list[[j]]@Name){
          ind <- c(ind, j)
        }
      }
    }

    hv_list_test <-  hv_list[[ind]]
    intersection <- hypervolume_set_n_intersection(hv_list_test)
    combi_df[[nb_combi+1]][i] <- intersection@Volume
  }

  #rescale
  combi_df[nb_combi+1] <- as.numeric(combi_df[[nb_combi+1]])
  rescaled_combi_df <-cbind(combi_df[1:nb_combi], apply(combi_df[nb_combi+1], MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))))

  return(rescaled_combi_df)
}

