#' Title
#'
#' @param hv_list a list of hypervolumes
#' @param abiotics_df data frame with abiotics data
#' @param user_param_syst user parameters
#' @param user_param_rescaled user parameters rescaled
#' @param species_list list of species
#'
#' @return
#' @export
#'
#' @examples
system_compatibility_app <- function(hv_list, abiotics_df, user_param_syst, user_param_rescaled, species_list){

  possible_combi <- c()
  for (i in 1:length(hv_list@HVList)){
    distance.factor = 1
    point_density = nrow(hv_list[[i]]@RandomPoints)/hv_list[[i]]@Volume
    cutoff_dist = point_density^(-1/ncol(hv_list[[i]]@RandomPoints)) * distance.factor

    test <- evalfspherical(data = hv_list[[i]]@RandomPoints, radius = cutoff_dist,
                           points =  user_param_rescaled[,-1] )

    if (test > 0){
      possible_combi <- c(possible_combi, hv_list[[i]]@Name)
    }

  }
  #############################################################################
  species_list_possible <- c()
  for (i in 1:length(possible_combi)){
    for (j in 1:length(species_list)){
      if (grepl(species_list[j],possible_combi[i])){
        species_list_possible <- c(species_list_possible, species_list[j])

      }
    }
  }

  species_list_possible <- unique(species_list_possible)

  #############################################################################

  shinyApp(ui = fluidPage(
    titlePanel("Species density"),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "Factor",
                    label = "Choose a factor:",
                    choices = selected_abiotics),

        checkboxGroupInput(inputId = "species_show",
                           label = "Chose species to show:",
                           choiceNames = species_list_possible,
                           choiceValues = species_list_possible)
      ),

      mainPanel(
        plotOutput("plot", width = "100%", height = 400),
        verbatimTextOutput("txt")
      )
    )
  ),

  server = function(input, output) {


    output$txt <- renderText(possible_combi, sep = "\n")

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
