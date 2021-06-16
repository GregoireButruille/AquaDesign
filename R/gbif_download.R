#' downloading gbif data
#'
#' @param species_list The list of species to be considered (Scientific names)
#' @param gbif_user Username on GBIF
#' @param gbif_pwd Password on GBIF
#' @param mail email address used for GBIF
#' @return A dataframe with all the occurence points of the species selected
#' @import taxize
#' @import rgbif
#' @export
#' @examples


gbif_download <- function(species_list, gbif_user, gbif_pwd, mail){
  species_gbifid <- get_gbifid_(species_list) #gives a list of lists with the 3 first results in gbif for each species
  gbif_taxon_keys<-c()
  for (i in 1:length(species_gbifid)) {
    if (species_gbifid[[c(i,5,1)]] == "EXACT"){ #for each species (i) check "matchtype" (5st list) to make sure that the wanted species corresponds to gbif first result (1)
      gbif_taxon_keys <- c(gbif_taxon_keys, species_gbifid[[c(i,1,1)]])  #add "usagekey" (1st list) to the list
    }
    else { #if the matchtype is not exact, suggest to change species name for gbif 1st result
      stop(paste0("!!! Warning !!! Species '",species_list[i],"' not found, please try again with : ",species_gbifid[[c(i,2)]])) #if the species does not exactly match with gbif 1st result, the algorithm suggest to try first result's "scientificname" (2nd list)
    }
  }
  ###Download the data from GBIF,enter username/password/email. Only keeping data with coordinates (The data is loaded on gbif and ready to download)
  data <- occ_download(pred_in("taxonKey", gbif_taxon_keys), pred("hasCoordinate", TRUE), format = "SIMPLE_CSV",user, pwd, email)

  return(data)
}
