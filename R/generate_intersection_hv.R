#' Title Generate a list of hypervolumes corresponding to the intersections of the input hypervolume list.
#'
#' @param hv_list list of species hypervolumes
#'
#' @return
#' @export
#'
#' @examples
generate_intersection_hv <- function (hv_list){

  nb_combi <- dlg_list(title = "Chose the max number of species in combinations",
                       c(2:(length(hv_list@HVList))))$res
  nb_combi <- as.numeric(nb_combi)
  species_list_hv_selected <- c()
  for (i in 1:length(hv_list@HVList)) {
    species_list_hv_selected <- c(species_list_hv_selected,
                                  hv_list[[i]]@Name)
  }
  list_combi <- do.call("c", lapply(1:nb_combi, function(i) combn(species_list_hv_selected,
                                                                  i, FUN = list)))
  list_combi <- list_combi[-(1:length(species_list_hv_selected))]
  print(paste0("combinations to calculate :", as.character(length(list_combi))))

  hv_intersection_list <- new("HypervolumeList")
  combi_df <- data.frame(matrix(ncol = nb_combi, nrow = 0))
  for (i in 1:length(list_combi)) {
    vect <- list_combi[[i]]
    length(vect) <- nb_combi + 1
    combi_df <- rbind(combi_df, vect)
  }
  combi_df[is.na(combi_df)] <- "None"

  for (i in 1:length(combi_df[, 1])) {
    ind <- c()
    hv_list_test <- new("HypervolumeList")
    for (j in 1:length(species_list_hv_selected)) {
      for (q in 1:length(combi_df[1, ])) {
        if (combi_df[[q]][i] == hv_list[[j]]@Name) {
          ind <- c(ind, j)
        }
      }
    }

    hv_list_test <- hv_list[[ind]]
    intersection <- hypervolume_set_n_intersection(hv_list_test)

    #set intersection hypervolumes names
    hv_name <- c()
    for (k in 1:length(list_combi[[i]])){
      hv_name <- paste(hv_name,list_combi[[i]][k])
    }
    intersection@Name <- hv_name

    hv_intersection_list <- hypervolume_join(hv_intersection_list, intersection)
  }

  return(hv_intersection_list)
}
