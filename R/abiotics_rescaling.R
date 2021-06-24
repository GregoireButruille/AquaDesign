#' Title
#'
#' @param flo1k_data raster stack with data from FLO1K
#' @param worldclim_data  raster stack with data from WorldClim
#' @param earthenv_data  raster stack with data from EarthEnv
#' @param minlat Mininum latitude
#' @param maxlat Maximum latitude
#' @param minlong Minimum longitude
#' @param maxlong Maximum longitude
#' @param resolution Resolution
#' @param geosphere If TRUE, import data from geosphere, which take a lot of time
#' @import doParallel
#' @import foreach
#' @importFrom geosphere daylength
#' @import parallel
#' @return
#' @export
#'
#' @examples
#'
abiotics_rescaling <- function(flo1k_data,worldclim_data,earthenv_data, minlat, maxlat, minlong, maxlong, resolution, geosphere = FALSE){

  cl <- makePSOCKcluster(detectCores()-2)
  registerDoParallel(cl)
  getDoParWorkers()

  cat("EarthEnv  1/4...\n")

  abiotics_df <- aggregate(earthenv_data[[1]], fact=2*resolution,fun=mean)
  abiotics_df <- as.data.frame(abiotics_df, xy = TRUE, centroids = TRUE)

  abiotics_df <- foreach(i=2:nlayers(earthenv_data), .combine = merge, .packages = c("raster", "ncdf4")) %dopar% {
    options(rasterNCDF4 = TRUE)
    tmp <- aggregate(earthenv_data[[i]], fact=2*resolution,fun=mean)   #60 arcsecs * 60 = 30 arcmins    #60 arcsecs * 20 = 10 arcmins
    tmp_df <- as.data.frame(tmp, xy = TRUE, centroids = TRUE)
    is.num <- sapply(tmp_df, is.numeric)
    tmp_df[is.num] <- lapply(tmp_df[is.num], round, 4)
    is.num <- sapply(abiotics_df, is.numeric)
    abiotics_df[is.num] <- lapply(abiotics_df[is.num], round, 4)
    abiotics_df <- merge(abiotics_df, tmp_df, by = c("x", "y"))
  }

  cat("FLO1K  2/4...\n")

  for (i in 1:length(flo1k_data)){
    flo1k_files_names <- c("av", "mi", "ma")

    if (resolution == 10){
      flow <- aggregate(flo1k_data [[i]], fact = 2, fun = mean) #fact = 2 to pass from 5 to 10 arcmins
    }

    if (resolution == 30){
      flow <- flo1k_data [[i]]
    }

    flow_df <- as.data.frame(flow, xy = TRUE, centroids = TRUE)
    names(flow_df)<- paste0(c("x", "y", paste0("flow_df_",flo1k_files_names[i])))
    is.num <- sapply(flow_df, is.numeric)
    flow_df[is.num] <- lapply(flow_df[is.num], round, 4)
    abiotics_df <- merge(abiotics_df, flow_df, xy = TRUE, centroids = TRUE, by = c("x", "y"))
  }

  cat("WorldClim  3/4...\n")

  if (resolution == 30){
    srad<-aggregate(worldclim_data[[i]], fact=3,fun=mean)     #fact = 3 to pass from 10 to 30 arcmins
    vapr<-aggregate(worldclim_data[[i]], fact=3,fun=mean)
    prec<-aggregate(worldclim_data[[i]], fact=3,fun=mean)
  }

  if (resolution == 10){
    srad <- worldclim_data[[i]]
    vapr <- worldclim_data[[i]]
    prec <- worldclim_data[[i]]
  }

  annual_prec <- prec[[1]]
  prec_WM <- prec[[2]]
  prec_DM <- prec[[3]]
  prec_seasonality <- prec[[4]]

  wc_files = c(srad, vapr)
  wc_files_names = c("srad", "vapr")
  wc_files2 = c(annual_prec, prec_WM, prec_DM, prec_seasonality)
  wc_files2_names = c("annual_prec", "prec_WM","prec_DM", "prec_seasonality")

  for(i in 1:length(wc_files)){
    var_mean <- calc(wc_files[[i]], fun = mean)
    df <- as.data.frame(var_mean, xy = TRUE, centroids = TRUE)
    names(df)<- paste(c("x", "y", wc_files_names[i]))
    is.num <- sapply(df, is.numeric)
    df[is.num] <- lapply(df[is.num], round, 4)
    abiotics_df <- merge(abiotics_df, df, xy = TRUE, centroids = TRUE, by = c("x", "y"))
  }

  for(i in 1:length(wc_files2)){
    df <- as.data.frame(wc_files2[[i]], xy = TRUE, centroids = TRUE)
    names(df)<- paste(c("x", "y", wc_files2_names[i]))
    is.num <- sapply(df, is.numeric)
    df[is.num] <- lapply(df[is.num], round, 4)
    abiotics_df <- merge(abiotics_df, df, xy = TRUE, centroids = TRUE, by = c("x", "y"))
  }

  if (geosphere == TRUE){
    cat("geosphere  4/4...\n")

    dl_min_df <- data.frame(matrix(ncol = 3, nrow = 0))
    dl_max_df <- data.frame(matrix(ncol = 3, nrow = 0))
    dl_range_df <- data.frame(matrix(ncol = 3, nrow = 0))

    #loop that calculate min/max/range for each latitude
    #/!\ takes a long time /!\
    if (resolution == 30){

      ymin = -55.75
      ymax =59.75
      xmin  = -144.75
      xmax = 179.75
      step = 0.5


      for (y in seq(ymin,ymax,step)){

        min <- min(daylength(y, 1:365))
        max <- max(daylength(y, 1:365))
        range <- max-min

        for (x in seq(xmin,xmax,step)){
          dl_min <- c(x, y, min)
          dl_max <- c(x, y, max)
          dl_range <- c(x, y, range)

          dl_min_df <- rbind(dl_min_df, dl_min)
          dl_max_df <- rbind(dl_max_df, dl_max)
          dl_range_df <- rbind(dl_range_df, dl_range)
        }
      }
    }


    if (resolution == 10){

      ky = 1
      kx = 1
      ymin = -55.91667
      ymax =59.91667
      xmin  = -144.9167
      xmax = 179.9167
      y = ymin
      x = xmin

      while (y < ymax){
        x = xmin

        min <- min(daylength(y, 1:365))
        max <- max(daylength(y, 1:365))
        range <- max-min

        while (x < xmax){
          dl_min <- c(x, y, min)
          dl_max <- c(x, y, max)
          dl_range <- c(x, y, range)

          dl_min_df <- rbind(dl_min_df, dl_min)
          dl_max_df <- rbind(dl_max_df, dl_max)
          dl_range_df <- rbind(dl_range_df, dl_range)

          if (kx == 3){
            x = x+0.1666
            kx = 0
          }
          else {
            x = x+0.1667
          }
          kx = kx + 1

        }

        if (ky == 3){
          y = y+0.16666
          ky = 0
        }
        else {
          y = y+0.16667
        }
        ky = ky + 1
      }

    }

    colnames(dl_min_df) <- c("x", "y", paste0("dl_annual_min"))
    colnames(dl_max_df) <- c("x", "y", paste0("dl_annual_max"))
    colnames(dl_range_df) <- c("x", "y", paste0("dl_annual_range"))

    is.num <- sapply(dl_min_df, is.numeric)
    dl_min_df[is.num] <- lapply(dl_min_df[is.num], round, 4)
    is.num <- sapply(dl_max_df, is.numeric)
    dl_max_df[is.num] <- lapply(dl_max_df[is.num], round, 4)
    is.num <- sapply(dl_range_df, is.numeric)
    dl_range_df[is.num] <- lapply(dl_range_df[is.num], round, 4)

    abiotics_df <- merge(abiotics_df, dl_min_df, by = c("x", "y"))
    abiotics_df <- merge(abiotics_df, dl_max_df, by = c("x", "y"))
    abiotics_df <- merge(abiotics_df, dl_range_df, by = c("x", "y"))
  }

  #finalize

  abiotics_df <- abiotics_df %>%
    filter(y<maxlat, y>(minlat), x>(minlong), x<maxlong)
  abiotics_df <-na.omit(abiotics_df) #remove NA values
  abiotics_df <-subset(abiotics_df, ph_max!=0) #remove null ph values

  return(abiotics_df)
}
