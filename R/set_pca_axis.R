#' Title Convert dataframe into PCA projection
#'
#' @param species_abiotics_df data frame with species and abiotics
#'
#'
#' @return
#' @export
#'
#' @examples
set_pca_axis <- function(species_abiotics_df){

  #Npc = number of principal components retained
  # Kaiser-Guttman rule
  res <- prcomp(species_abiotics_df[-1], scale = TRUE)
  Npc <- as.numeric(summary(res$sd^2 > 1)["TRUE"])
  print(paste("using",Npc,"axes"))

  rescaled_abiotics <- res$x[,1:Npc]

  rescaled_abiotics <- cbind(species_abiotics_df$species , as.data.frame(rescaled_abiotics))


  #set axes names
  axis_names <- c()
  for (i in 1:Npc){
    axis_names <- c(axis_names, paste0("Axis", i))
  }
  colnames(rescaled_abiotics) <- c("species", axis_names)

  return(rescaled_abiotics)
}
