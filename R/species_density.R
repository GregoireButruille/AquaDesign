#' Title Plot species density diagrams for individual species
#'
#' @param abiotics_df
#' @param species_list
#'
#' @return
#' @export
#'
#' @examples
design_monoculture_app <- function (species_abiotics_df, species_list, minlat = -56, maxlat = 60 , minlong =-145, maxlong = 180){

  #get abiotics list from the dataframe
  selected_abiotics <- as.list(colnames(species_abiotics_df[, -1]))

  #set abiotics names to display
  names(selected_abiotics) <- c("Annual mean temperature (?C)",
                                "Maximum temperature of the warmest month (?C)",
                                "Temperature annual range (°C)",
                                "Maximum pH of the soil ",
                                "Temperature seasonnality",
                                "Minimum temperature of the coldest month (?C)",
                                "Mean temperature of the driest quarter (?C)",
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
  species_abiotics_df$annual_meanT <- abiotics_df$annual_meanT/10
  species_abiotics_df$maxT_WM <- abiotics_df$maxT_WM/10
  species_abiotics_df$annual_rangeT <- abiotics_df$annual_rangeT/10
  species_abiotics_df$ph_max <- abiotics_df$ph_max/10
  species_abiotics_df$minT_CM <- abiotics_df$minT_CM/10
  species_abiotics_df$meanT_DQ <- abiotics_df$meanT_DQ/10

  species_abiotics_df <- species_abiotics_df %>%
    dplyr::filter(y<maxlat, y>(minlat), x>(minlong), x<maxlong)

  shinyApp(ui = fluidPage(titlePanel("Species density"),
                          sidebarLayout(

                            sidebarPanel(

                              #Choice of the parameter
                              selectInput(inputId = "Factor",
                                          label = "Choose a factor:",
                                          choices = selected_abiotics),

                              #choice of the species to show
                              checkboxGroupInput(inputId = "species_show",
                                                 label = "Chose species to show:",
                                                 choiceNames = species_list,
                                                 choiceValues = species_list)
                            ),

                            #plotting of the density diagram
                            mainPanel(plotOutput("plot", width = "100%", height = 400)


                            )
                          )
  ),

  server = function(input, output) {

    output$plot <- renderPlot({
      #subset the dataframe to the species to show
      species_abiotics_df_sub <- species_abiotics_df %>%
        filter(species %in% input$species_show)
      #plot density diagram
      ggplot(species_abiotics_df_sub, aes(x = species_abiotics_df_sub[,input$Factor], fill = species))+
        geom_density(alpha = 0.4) +
        xlab(names(selected_abiotics[which(selected_abiotics %in%
                                             input$Factor)]))
    })
  }
  )
}
