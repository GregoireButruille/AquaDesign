#' Title
#'
#' @param databases_to_use List of databases to use
#' @param resolution Resolution
#'
#'@importFrom utils download.file
#'@import svDialogs
#' @return
#' @export
#'
#' @examples
download_DB <- function(databases_to_use, resolution){

  if ("EarthEnv" %in% databases_to_use ){

    cat("Downloading datasets from EarthEnv...\n")

    download.file("https://data.earthenv.org/streams/hydroclim_average+sum.nc",
                  paste(getwd(), "hydro_avg.nc", sep="/"), mode = "wb")
    download.file("https://data.earthenv.org/streams/soil_maximum.nc",
                  paste(getwd(), "soil_max.nc", sep="/"), mode = "wb")
    download.file("https://data.earthenv.org/streams/elevation.nc",
                  paste(getwd(), "elevation.nc", sep="/"), mode = "wb")
    download.file("https://data.earthenv.org/streams/slope.nc",
                  paste(getwd(), "slope.nc", sep="/"), mode = "wb")
  }

  if ("WorldClim" %in% databases_to_use ){

    cat("Downloading datasets from WorldClim...\n")

    download.file("https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_10m_srad.zip", paste(getwd(), "srad_zip.zip", sep="/"), mode = "wb")
    download.file("https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_10m_vapr.zip", paste(getwd(), "vapr_zip.zip", sep="/"), mode = "wb")
    download.file("https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_10m_bio.zip", paste(getwd(), "bio_zip.zip", sep="/"), mode = "wb")
  }

  if ("FLO1K" %in% databases_to_use ){

    cat("Downloading datasets from FLO1K...")

    if (resolution == 30){
      download.file("https://ndownloader.figshare.com/files/10597966", paste(getwd(), "flow_30_zip.zip", sep="/"), mode = "wb")
    }

    if (resolution == 10){
      download.file("https://ndownloader.figshare.com/files/10597972", paste(getwd(), "flow_5_zip.zip", sep="/"), mode = "wb")
    }
  }

}
