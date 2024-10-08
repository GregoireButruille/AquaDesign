#' Title Exctract chosen paramters from WorldClim
#'
#' @return
#' @export
#'
#' @examples
WorldClim_select <- function(){
  #create a list of index to use in the loop
  months <-c("01","02","03","04","05","06","07","08","09","10","11","12")
  bio <-c("12", "13", "14", "15")

  #raster brick can't have different files in them, so this code is obsolete (it had the weird effect on not working the first time this code runs, and working by creating a raster stack the second time)
  #create empty raster brick with 12 layers, with nrow and ncol corresponding to 10 arcmins grid (before spatial filtering)
  #srad<-brick(nrows=1080,ncol=2160,nl=length(months))
  #vapr<-brick(nrows=1080,ncol=2160,nl=length(months))
  #prec<-brick(nrows=1080,ncol=2160,nl=length(bio))

  #Unzip each layer/month, aggregate it and rename it
  #for(i in 1:length(months)){
  #  name_srad <-paste0("wc2.1_10m_srad_",months[i],".tif")
  #  srad[[i]]<-raster(unzip("srad_zip.zip",name_srad))
  #  name_vapr <-paste0("wc2.1_10m_vapr_",months[i],".tif")
  #  vapr[[i]]<-raster(unzip("vapr_zip.zip",name_vapr))
  #}

  #Unzip each layer/month and rename it
  #for(i in 1:length(bio)){
  #  name_bio <-paste0("wc2.1_10m_bio_",bio[i],".tif")
  #  prec[[i]]<-raster(unzip("bio_zip.zip",name_bio))
  #}

  #new version
  srad_names=list()
  vapr_names=list()
  prec_names=list()

  for(i in 1:length(months)){
    name_srad <-paste0("wc2.1_10m_srad_",months[i],".tif")
    name_vapr <-paste0("wc2.1_10m_vapr_",months[i],".tif")
    unzip("srad_zip.zip",name_srad)
    unzip("vapr_zip.zip",name_vapr)
    srad_names <- c(srad_names, name_srad)
    vapr_names <- c(vapr_names, name_vapr)
  }

  for(i in 1:length(bio)){
    name_bio <-paste0("wc2.1_10m_bio_",bio[i],".tif")
    unzip("bio_zip.zip",name_bio)
    prec_names <- c(prec_names, name_bio)
  }
  srad <-stack(srad_names)
  vapr <-stack(vapr_names)
  prec <-stack(prec_names)

  #weird double assignation here
  #wc_files = c(srad, vapr, prec)
  #result <- wc_files

  result = c(srad, vapr, prec)
  return(result)

}
