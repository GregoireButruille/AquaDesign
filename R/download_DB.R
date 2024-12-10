#' Title Download datasets from various databases
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

  checkAndDownload <- function(url,filePath) {
      if (!file.exists(filePath)) {
        tryCatch(download.file(url,filePath, mode = "wb", quiet = FALSE), 
          error = function(e) print(paste0("The download failed on url ",url)))
          quit()
      }
      else{
        warningMsg=paste0("The file ",filePath," already exist. If you wish to update it, please remove this file and rerun Aquadesign.")
        cat(warningMsg)
      }
  }
    
  options(timeout = 1000)
  workingDir=getwd()

  hydroUrl="https://data.earthenv.org/streams/hydroclim_average+sum.nc"
  soilUrl="https://data.earthenv.org/streams/soil_maximum.nc"
  elevationUrl="https://data.earthenv.org/streams/elevation.nc"
  slopeUrl="https://data.earthenv.org/streams/slope.nc"

  #old urls
  #https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_10m_srad.zip
  #https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_10m_vapr.zip
  #https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_10m_bio.zip
  sradUrl="https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_10m_srad.zip"
  vaprUrl="https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_10m_vapr.zip"
  bioUrl="https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_10m_bio.zip"
  
  flow30Url="https://ndownloader.figshare.com/files/10597966"
  flow10Url="https://ndownloader.figshare.com/files/10597972"

  hydroFilepath=file.path(workingDir,"hydro_avg.nc")
  soilFilepath=file.path(workingDir,"soil_max.nc")
  elevationFilepath=file.path(workingDir,"elevation.nc")
  slopeFilepath=file.path(workingDir,"slope.nc")

  sradFilepath=file.path(workingDir,"srad_zip.zip")
  vaprFilepath=file.path(workingDir,"vapr_zip.zip")
  bioFilepath=file.path(workingDir,"bio_zip.zip")

  flow30Filepath=file.path(workingDir,"flow_30_zip.zip")
  flow10Filepath=file.path(workingDir,"flow_5_zip.zip")  
  
  if ("EarthEnv" %in% databases_to_use ){

    cat("Downloading datasets from EarthEnv...\n")

    checkAndDownload(hydroUrl,hydroFilepath)
    checkAndDownload(soilUrl,soilFilepath)
    checkAndDownload(elevationUrl,elevationFilepath)
    checkAndDownload(slopeUrl,slopeFilepath)
  }

  if ("WorldClim" %in% databases_to_use ){

    cat("Downloading datasets from WorldClim...\n")

    checkAndDownload(sradUrl,sradFilepath)
    checkAndDownload(vaprUrl,vaprFilepath)
    checkAndDownload(bioUrl,bioFilepath)
  }

  if ("FLO1K" %in% databases_to_use ){

    cat("Downloading datasets from FLO1K...")

    if (resolution == 30){
      checkAndDownload(flow30Url,flow30Filepath)
    }

    if (resolution == 10){
      checkAndDownload(flow10Url,flow10Filepath)
    }
  }

}
