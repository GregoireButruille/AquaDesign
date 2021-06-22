#' Title
#'
#' @param nb_combi Maximum number of species in a combination
#' @param hv_list A list of hypervolumes
#' @param species_list_hv_selected List of the species represented by the hypervolumes
#'
#' @return
#' @export
#'
#' @examples
hypervolume_overlap <- function(hv_list){

  nb_combi <- dlg_list(title = "Chose the max number of species in combinations", c(2:(length(hv_list@HVList))))$res
  nb_combi <- as.numeric(nb_combi)

  species_list_hv_selected <- c()
  for (i in 1:length(hv_list@HVList)){
    species_list_hv_selected <- c(species_list_hv_selected, hv_list[[i]]@Name)
  }

  list_combi <- do.call("c", lapply(1:nb_combi, function(i) combn(species_list_hv_selected, i, FUN = list)))
  list_combi <- list_combi[-(1:length(species_list_hv_selected))]

  warning(paste0("combinations to calculate :" , as.character(length(list_combi))))


  combi_df <- data.frame(matrix(ncol = nb_combi, nrow = 0))

  for ( i in 1:length(list_combi)){
    vect <- list_combi[[i]]
    length(vect) <- nb_combi+1
    combi_df <- rbind(combi_df, vect)
  }

  combi_df[is.na(combi_df)] <- "None"

  for (i in 1:length(combi_df[,1])){
    ind <- c()
    hv_list_test <- new("HypervolumeList")

    for (j in 1:length(species_list_hv_selected)){
      for (q in 1:length(combi_df[1,])){

        if (combi_df[[q]][i] == hv_list[[j]]@Name){
          ind <- c(ind, j)
        }
      }
    }

    hv_list_test <-  hv_list[[ind]]
    intersection <- HV_intersection(hv_list_test)
    combi_df[[nb_combi+1]][i] <- intersection@Volume
  }

  #rescale
  combi_df[nb_combi+1] <- as.numeric(combi_df[[nb_combi+1]])
  rescaled_combi_df <-cbind(combi_df[1:nb_combi], apply(combi_df[nb_combi+1], MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))))

  return(rescaled_combi_df)
}

