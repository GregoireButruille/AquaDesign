#' Title
#'
#' @param abiotics_df data frame with abiotics data
#' @param species_df data frame with species occurrences
#' @param selected_abiotics list of the selected abiotic variables
#'
#' @return
#' @export
#'
#' @examples
abiotics_species_merge <- function(abiotics_df, species_df, selected_abiotics) {
  #select variables from abiotics df to have x and y  (annual_rangeT, annual_meanT, annual_prec_wc, ph_max, maxT_WM, srad_30, prec_DM_wc, prec_seasonality_wc, flow_avg (range?), elevation, slope)
  selected_abiotics_df <- cbind(abiotics_df[c(1,2)], abiotics_df[selected_abiotics])

  #merge and crop datasets to obtain columns as : Species / var 1 / var 2 / ...
  species_abiotics_df <- data.frame(matrix(ncol = length(names(selected_abiotics_df)), nrow = 0))
  column_names <- names(selected_abiotics_df)[3:length(names(selected_abiotics_df))]
  colnames(species_abiotics_df) <- c("species", column_names)

  for (i in 1:length(species_list)){
    species <- species_df[species_df$species==unique(species_df$species)[i],]
    species_name <- merge(species, selected_abiotics_df, by=c("x","y")) #merge species i with factors data frame
    colnames(species_name)[3] <- c("species")                          #rename "species name" column
    species_name$species[species_name$species==1] <- as.character(unique(species_df$species)[i]) #replace cell with value = 1 with the species name
    species_name <-na.omit(species_name)                                #remove NA
    species_name <- species_name[,-(1:2)]                               #remove x and y columns
    species_abiotics_df <-rbind(species_abiotics_df, species_name)
  }
  return(species_abiotics_df)
}
