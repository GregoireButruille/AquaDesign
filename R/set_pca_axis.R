#' Title
#'
#' @param species_abiotics_df data frame with species and abiotics
#'
#'@import ade4
#' @return
#' @export
#'
#' @examples
set_pca_axis <- function(species_abiotics_df){

  nb_variables <- length(species_abiotics_df)-1

  #rescale between 0 and 1
  rescaled_abiotics <- cbind(species_abiotics_df[1],apply(species_abiotics_df[2:nb_variables+1], MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))))
  rescaled_abiotics_save <- rescaled_abiotics

  rescaled_abiotics$species = as.numeric(as.factor(rescaled_abiotics$species))

  ####test package factoextra
  res <- prcomp(rescaled_abiotics, scale = TRUE)

  #Npc = number of principal components retained
  # Kaiser-Guttman rule
  Npc <- as.numeric(summary(res$sd^2 > 1)["TRUE"])
  print(paste("using",Npc,"axes"))

  #set pca using ade4 package
  res <- dudi.pca(df = rescaled_abiotics, scannf = FALSE, nf = Npc)
  rescaled_abiotics <- cbind(rescaled_abiotics_save$species , data.frame(res$li))

  #set axes names
  axis_names <- c()
  for (i in 1:Npc){
    axis_names <- c(axis_names, paste0("Axis", i))
  }
  colnames(rescaled_abiotics) <- c("species", axis_names)

  return(rescaled_abiotics)
}
