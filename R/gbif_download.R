#' downloading gbif data
#'
#' @param species_list The list of species to be considered (Scientific names)
#' @return A data frame with all the occurrence points of the species selected
#' @import taxize
#' @import rgbif
#' @import vroom
#' @export
#' @examples

gbif_download <- function(species_list,user=NA,pwd=NA,mail=NA,minlat=-90, maxlat=90, minlong=-180, maxlong=180){

  #if not given in the function, ask user for its GBIF credentials (id, pwd and mail)
  if (is.na(user)){
    gbif_user <- dlgInput("Enter your gbif username", "username")$res
  } else{
    gbif_user<-user
  }
  if (is.na(pwd)){
    gbif_pwd <- dlgInput("Enter your gbif password", "password")$res
  } else{
    gbif_pwd <- pwd
  }
  if (is.na(mail)){
    gbif_mail <- dlgInput("Enter the mail address used on gbif", "email@address.com")$res
  } else{
    gbif_mail <- mail
  }

  #search the ids of species in GBIF
  species_gbifid <- get_gbifid_(species_list) #returns a list of lists with the 3 first results in gbif for each species
  gbif_taxon_keys<-c()
  for (i in 1:length(species_gbifid)) {
    if (species_gbifid[[c(i,5,1)]] == "EXACT"){ #for each species (i) check "matchtype" (5st list) to make sure that the wanted species corresponds to gbif first result (1)
      gbif_taxon_keys <- c(gbif_taxon_keys, species_gbifid[[c(i,1,1)]])  #add "usagekey" (1st list) to the list
    }
    else { #if the matchtype is not exact, suggest to change species name to gbif 1st result
      stop(paste0("!!! Warning !!! Species '",species_list[i],"' not found, please try again with : ",species_gbifid[[c(i,2)]])) #if the species does not exactly match with gbif 1st result, the algorithm suggest to try first result's "scientificname" (2nd list)
    }
  }

  #prepare the download, wait for the data set to be ready then download the zip filen unzip it and read the csv
  #data <- occ_download(pred_in("taxonKey", gbif_taxon_keys), pred("hasCoordinate", TRUE), format = "SIMPLE_CSV",user=gbif_user, pwd=gbif_pwd, email=gbif_mail)
  data <- occ_download(pred_in("taxonKey", gbif_taxon_keys), pred("hasCoordinate", TRUE), pred_lt("decimalLatitude",maxlat), pred_gt("decimalLatitude",minlat), pred_lt("decimalLongitude",maxlong), pred_gt("decimalLongitude",minlong), format = "SIMPLE_CSV",user=gbif_user, pwd=gbif_pwd, email=gbif_mail)
  occ_download_wait(data)
  gbif_zip_download<-occ_download_get(data[1],overwrite=TRUE)
  print(gbif_citation(gbif_zip_download))

  #replacing the unzipping and csv reading with vroom, which has the ability of directly reading the zip, and only the columns I'm interested with
  #unzip(gbif_zip_download)
  #data <- read.csv(paste0(data[1],".csv"),header = TRUE, sep = "\t", quote = "")
  gbif_zip_path=paste0(data[1],".zip")
  data <-vroom(gbif_zip_path, quote="", col_select = c(species, decimalLongitude, decimalLatitude,gbifID,countryCode))
  #file.remove(gbif_zip_path)
  return(data)
}
