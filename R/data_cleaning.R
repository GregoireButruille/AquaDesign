#' Title Remove suspicious occurence points
#'
#' @param data A data frame downloaded from GBIF
#' @param minlat Minimum latitude
#' @param maxlat Maximum latitude
#' @param minlong Minimum longitude
#' @param maxlong Maximum longitude
#' @param check.out Check outliers or no (take a long time for a large number of occurences)
#'
#'@importFrom dplyr select
#'@importFrom dplyr filter
#'@import countrycode
#'@import CoordinateCleaner
#'@import rnaturalearthdata
#' @return
#' @export
#'
#' @examples
data_cleaning <- function(data, minlat, maxlat, minlong, maxlong, check.out = TRUE){

  #keep only the columns of interest and set spatial extent
  data <- data%>%
    dplyr::select(species, decimalLongitude, decimalLatitude, gbifID, countryCode)%>%
    filter(decimalLatitude<maxlat)%>%
    filter(decimalLatitude>(minlat))%>%
    filter(decimalLongitude<maxlong)%>%
    filter(decimalLongitude>(minlong))

  #prepare data for cleaning
  #an update to CoordinateCleaner passed the default column names "decimallongitude" and "decimallatitude" to "decimalLongitude" and "decimalLatitude", respectively, breaking the functions calls
  #hence, I'm removing the column renaming, keeping the correct ones
  #names(data)[2:3] <- c("decimallongitude", "decimallatitude") #these are default names for latitude and longitude in the following functions
  data$countryCode <-  countrycode(data$countryCode, origin =  'iso2c', destination = 'iso3c') #iso 2 --> iso 3 changes countrycode from 2 letters to 3 letters (ex : FR --> FRA) to be able to use cc_count()

  #country code puts Na for countrycodes not matched unambiguously (XK and ZZ = Kosovo and undefined countries), remove the Na
  data <- na.omit(data)

   #removes suspicious points (buffer = range in meters)
  #cc_coun is at least 30 minutes long (it didn't end after 30min), the other functions are mere seconds, there is probably a problem with it, so I'm removing it
  #cc_sea() is also long, launch a weird download, and we're treating sea water fishes sometimes
  data <- data%>%
    cc_val()%>%  #invalid values
    cc_cap(buffer=10000)%>%   #capitals
    cc_cen(buffer = 1000)%>%  #country centroids
  #  cc_coun(iso3 = "countryCode")%>%  #country mismatches
    cc_gbif(buffer=1000)%>%  #gbif HeadQuarters
  #  cc_inst(buffer=100)%>%  #institutions
    cc_inst(buffer=100)  #institutions
  #  cc_sea()  #sea
  

  if (check.out == TRUE){
    #check outliers
    data <- data%>%   #/!\ takes quite a long time /!\
      cc_outl(
        species = "species", #check species by species
        method = "distance",
        tdi = 1000,          #if a point is more than 1000km away from every other points from the same species, it is removed
        value = "clean",
        thinning = TRUE,
        thinning_res = 0.5
      )
  }

  #Renaming back latitude and longitude is obsolete since the columns stays at deciamlLongitude and decimalLatitude from the start
  #names(data)[2:3] <- c("decimalLongitude", "decimalLatitude")

  return(data)
}
