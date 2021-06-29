#' Title
#'
#' @return
#' @export
#' @import rnaturalearth
#' @examples

plot_occ <- function(gbif_data, data_cl){

  world <- ne_countries(scale = "medium", returnclass = "sf")
  species_list <- unique(data_cl$species)

  shinyApp(ui = fluidPage(

    titlePanel("Species distribution"),

    sidebarLayout(

      sidebarPanel(

        selectInput(inputId = "Species",
                    label = "Choose a species:",
                    choices = species_list)
      ),

      # Main panel for displaying outputs ----
      mainPanel(
        # Output: HTML table with requested number of observations ----
        plotOutput("map", width = "100%", height = 800)

      )
    )
  ),

  server = function(input, output) {

    observeEvent(input$Species,{
      data_filtered <- data[data$species==input$Species,]
      data_cl_filtered <- data_cl[data_cl$species==input$Species,]
      output$map <- renderPlot({
        ggplot(data = world)+
          geom_sf()+
          geom_point(data = data_filtered , aes(x = decimalLongitude, y = decimalLatitude), col = "red")+
          geom_point(data = data_cl_filtered , aes(x = decimalLongitude, y = decimalLatitude), col = "blue")

      })
    })

  })
}
