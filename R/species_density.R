#' Title Plot species density diagrams for individual species
#'
#' @param abiotics_df
#' @param species_list
#'
#' @return
#' @export
#'
#' @examples
design_monoculture_app <- function (abiotics_df, species_list){

  selected_abiotics <- as.list(colnames(species_abiotics_df[, -1]))

  #set abiotics names to display
  names(selected_abiotics) <- c("Annual mean temperature (?C*10)",
                                "Maximum temperature of the warmest month (?C*10)",
                                "Minimum temperature of the coldest month (?C*10)",
                                "Mean temperature of the driest quarter (?C*10)", "Temperature seasonnality",
                                "Temperature annual range", "Maximum pH of the soil (*10)",
                                "Average elevation (meters)", "Average slope", "Average flow",
                                "Minimum flow", "Maximum flow", "Solar radiation", "Water vapor pressure",
                                "Annual precipitations", "Precipitation of the wettest month",
                                "Precipitation of the driest month", "Precipitation seasonnality",
                                "Daylength annual min", "Daylength annual max", "Daylength annual range")

  shinyApp(ui = fluidPage(titlePanel("Species density"),
                          sidebarLayout(

                            sidebarPanel(
                              selectInput(inputId = "Factor",
                                          label = "Choose a factor:",
                                          choices = selected_abiotics),

                              checkboxGroupInput(inputId = "species_show",
                                                 label = "Chose species to show:",
                                                 choiceNames = species_list,
                                                 choiceValues = species_list)
                            ),

                            mainPanel(plotOutput("plot", width = "100%", height = 400)


                            )
                          )
  ),

  server = function(input, output) {

    output$plot <- renderPlot({
      species_abiotics_df_sub <- species_abiotics_df %>%
        filter(species %in% input$species_show)
      ggplot(species_abiotics_df_sub, aes(x = species_abiotics_df_sub[,input$Factor], fill = species))+
        geom_density(alpha = 0.4) +
        xlab(names(selected_abiotics[which(selected_abiotics %in%
                                             input$Factor)]))
    })
  }
  )
}
