#' Title
#'
#' @param data_cl GBIF cleaned data frame
#' @param minlat Minimum latitude
#' @param maxlat Maximum latitude
#' @param minlong Minimum longitude
#' @param maxlong Maximum longitude
#' @param resolution Resolution
#'
#'@import raster
#'@import progress
#'@import sf
#' @return
#' @export
#'
#' @examples
occ_rescaling <- function(data_cl, minlat, maxlat , minlong, maxlong){

  resolution <- dlg_list(title = "Chose resolution (arcmins)", c(10,30))$res
  resolution <- as.numeric(resolution)

  species_df<- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(species_df) <- c("x", "y", "species")

  point<-st_as_sf(data_cl, coords = c("decimalLongitude", "decimalLatitude"), crs ="WGS84")
  # Create an empty raster (vals = 0) at 30 arcmins (res = 0.5)  in WGS84 (crs(point))
  if (resolution == 30){
    rast <- raster(crs = crs(point), vals=0,resolution = c(0.5, 0.5))
    # Rasterization (Presence map) and stocking the results in a multi-layers raster brick
    brick_rast <- brick(nrows=nrow(rast),ncol=ncol(rast),nl=length(unique(point$species))) #nl = create as many layers as different species
  }

  if (resolution == 10){
    rast <- raster(crs = crs(point), vals=0,resolution = c(1/6, 1/6))
    # Rasterization (Presence map) and stocking the results in a multi-layers raster brick
    brick_rast <- brick(nrows=nrow(rast),ncol=ncol(rast),nl=length(unique(point$species))) #nl = create as many layers as different species
  }

  #set a progress bar
  pb <- progress_bar$new(total = length(unique(point$species)))
  pb$tick(0)

  #for each species, rasterize the occurence points in 30 arcmins rasterbricks and get a dataframe
  for(i in 1:length(unique(point$species))){ #might be long
    if (!pb$finished==TRUE){
      pb$update(i/length(unique(point$species)))
    }
    point.i<-point[point$species==unique(point$species)[i],] #point.i is a "sf" dataframe with all the occurence pints of species i
    brick_rast[[i]]<-rasterize(as(point.i, "Spatial"),rast,field=1)
    df <- as.data.frame(brick_rast[[i]], xy=TRUE, centroids = TRUE)
    df <- df %>%
      filter(y<maxlat, y>(minlat), x>(minlong), x<maxlong) #spatial filter (already done before)
    colnames(df)[3] <- c("species")
    df$species[df$species==1] <- as.character(unique(point$species)[i]) #if the species is present(=1) replace "1" with species name
    df <-na.omit(df) #remove NA
    species_df <- rbind(species_df, df) #add to the global dataframe
  }

  pb$terminate()

  return(species_df)
}
