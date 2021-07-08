#' Title
#'
#' @param abiotics_df data frame with abiotics data
#'
#' @return
#' @export
#'
#' @examples
input_user_system <- function(abiotics_df){

  selected_abiotics <- as.list(colnames(abiotics_df[,-(1:2)]))

  names(selected_abiotics)  <- c("Annual mean temperature (°C*10)", "Maximum temperature of the warmest month (°C*10)", "Minimum temperature of the coldest month (°C*10)", "Mean temperature of the driest quarter (°C*10)", "Temperature seasonnality", "Temperature annual range", "Maximum pH of the soil (*10)", "Average elevation (meters)","Average slope", "Average flow", "Minimum flow", "Maximum flow","Solar radiation", "Water vapor pressure", "Annual precipitations", "Precipitation of the wettest month", "Precipitation of the driest month","Precipitation seasonnality","Daylength annual min","Daylength annual max", "Daylength annual range")

  user_lat <- as.numeric(dlgInput("Enter your site location latitude", "47.75")$res)
  user_long <- as.numeric(dlgInput("Enter your site location longitude", "6.2500")$res)
  user_coords <- data.frame(y = user_lat, x =user_long)
  user_param <- merge(user_coords, abiotics_df, by=c("x","y"))
  user_param <- user_param[,-(1:2)]
  user_param_edited <- user_param

  for (i in 1:length(user_param[1,])){
    user_val <- as.numeric(dlgInput(names(selected_abiotics[i]), as.character(user_param[i]))$res)
    user_param_edited[i] <- as.numeric(user_val)
  }

  return(user_param_edited)

}
