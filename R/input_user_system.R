#' Title Register the parameters corresponding to user's system
#'
#' @param abiotics_df data frame with abiotics data
#'
#' @return
#' @export
#'
#' @examples
input_user_system <- function(abiotics_df){



  selected_abiotics <- as.list(colnames(abiotics_df[,-(1:2)]))

  #set abiotics names to display
  names(selected_abiotics) <- c("Annual mean temperature (°C)",
                                "Maximum temperature of the warmest month (°C)",
                                "Temperature annual range (°C)",
                                "Maximum pH of the soil",
                                "Temperature seasonnality",
                                "Minimum temperature of the coldest month (°C)",
                                "Mean temperature of the driest quarter (°C)",
                                "Average elevation (meters)",
                                "Average slope([°]*100)", "Average flow (m3.s-1)",
                                "Minimum flow (m3.s-1)",
                                "Maximum flow (m3.s-1)",
                                "Solar radiation (kJ.m-2.day-1)",
                                "Water vapor pressure (kPa)",
                                "Annual precipitations (mm)",
                                "Precipitation of the wettest month (mm)",
                                "Precipitation of the driest month (mm)",
                                "Precipitation seasonnality",
                                "Daylength annual min (Hours)",
                                "Daylength annual max (Hours)",
                                "Daylength annual range (Hours)")


  #divide temperatures and pH by 10
  abiotics_df$annual_meanT <- abiotics_df$annual_meanT/10
  abiotics_df$maxT_WM <- abiotics_df$maxT_WM/10
  abiotics_df$annual_rangeT <- abiotics_df$annual_rangeT/10
  abiotics_df$ph_max <- abiotics_df$ph_max/10
  abiotics_df$minT_CM <- abiotics_df$minT_CM/10
  abiotics_df$meanT_DQ <- abiotics_df$meanT_DQ/10


  #ask for latitude and longitude
  user_lat <- as.numeric(dlgInput("Enter your site location latitude", "47.75")$res)
  user_long <- as.numeric(dlgInput("Enter your site location longitude", "6.2500")$res)

  #get the closest cell to coordinates
  rank <- which(abs(abiotics_df$y - user_lat) == min(abs(abiotics_df$y - user_lat))&abs(abiotics_df$x - user_long) == min(abs(abiotics_df$x - user_long)))
  user_lat <- abiotics_df$y[rank]
  user_long <- abiotics_df$x[rank]



  #get the line of the abiotics_df corresponding to these coordinates
  user_coords <- data.frame(y = user_lat, x =user_long)
  user_param <- merge(user_coords, abiotics_df, by=c("x","y"))
  user_param <- user_param[,-(1:2)] #remove x and y
  user_param_edited <- user_param

  #for each parameter, ask the user if they want to edit
  for (i in 1:length(user_param[1,])){
    user_val <- as.numeric(dlgInput(names(selected_abiotics[i]), as.character(user_param[i]))$res)
    user_param_edited[i] <- as.numeric(user_val)
  }

  #multiply temperatures and pH by 10 for the analysis
  user_param_edited[1] <-  user_param_edited[1]*10
  user_param_edited[2] <-  user_param_edited[2]*10
  user_param_edited[3] <-  user_param_edited[3]*10
  user_param_edited[4] <-  user_param_edited[4]*10
  user_param_edited[6] <-  user_param_edited[6]*10
  user_param_edited[7] <-  user_param_edited[7]*10


  return(user_param_edited)

}
