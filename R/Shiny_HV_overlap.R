#' Title
#'
#' @param species_list_hv_selected List of species selected
#' @param nb_combi Maximum number of species in a combinationn
#' @param selected_abiotics list of selected abiotics
#' @param rescaled_combi_df Data frame with rescaled compatibility index
#' @param species_abiotics_df dataframe with all abiotic variables
#'
#'
#' @return
#' @import ggplot2
#' @export
#'
#' @examples
Shiny_HV_overlap <- function(nb_combi, selected_abiotics, rescaled_combi_df, species_abiotics_df){

  nb_combi <- dlg_list(title = "Chose the max number of species in combinations", c(2:(length(species_list_hv_selected))))$res
  nb_combi <- as.numeric(nb_combi)

  species_list_hv_selected <- c()
  for (i in 1:length(hv_list@HVList)){
    species_list_hv_selected <- c(species_list_hv_selected, hv_list[[i]]@Name)
  }


  shinyApp(

    ui = fluidPage(

      titlePanel("Species compatibility"),

      sidebarLayout(

        sidebarPanel(

          selectInput(inputId = "central_species",
                      label = "Choose a central species:",
                      choices = c("None", species_list_hv_selected)),

          selectInput(inputId = "nb_species",
                      label = "Choose a number of species:",
                      choices = 2:as.numeric(nb_combi)),

          selectInput(inputId = "nb_combi_display",
                      label = "Choose the number of best combinations to display:",
                      choices = 5:50),

          selectInput(inputId = "Factor",
                      label = "Choose a factor:",
                      choices = selected_abiotics),

          checkboxGroupInput(inputId = "species_show",
                             label = "Chose species to show:",
                             choiceNames = species_list_hv_selected,
                             choiceValues = species_list_hv_selected)

        ),


        # Main panel for displaying outputs ----
        mainPanel(
          # Output: HTML table with requested number of observations ----
          plotOutput("plot1", width = "100%", height = 600),
          plotOutput("plot2", width = "100%", height = 400)
        )
      )

    ),


    #### set server #####
    server = function(input, output) {

      selected_species <- species_list_hv_selected


      #static background map
      output$plot1 <- renderPlot({

        if (input$central_species == "None"){

          combi_df_sp <- data.frame(matrix(ncol = as.numeric(input$nb_species), nrow = 0))
          for (i in 1:length(rescaled_combi_df[,1])){
            n <- rowSums(rescaled_combi_df == "None")
            if ((nb_combi - n[i]) == as.numeric(input$nb_species)){
              vect <- rescaled_combi_df[i,]
              combi_df_sp <- rbind(combi_df_sp, vect)
            }
          }
          best_combi <- combi_df_sp %>% arrange(desc(as.numeric(combi_df_sp[[nb_combi+1]]))) %>% ## on classe par ordre d?croissant
            slice(1:input$nb_combi_display) ## on s?lectionne les 5 premi?res lignes (les plus hautes)

        }

        else{
          row_sub = apply(rescaled_combi_df, 1, function(row) all(row != input$central_species ))
          combi_df_sub <- rescaled_combi_df[!row_sub,]

          combi_df_central_sp <- data.frame(matrix(ncol = as.numeric(input$nb_species), nrow = 0))
          for (i in 1:length(combi_df_sub[,1])){
            n <- rowSums(combi_df_sub == "None")
            if ((nb_combi - n[i]) == as.numeric(input$nb_species)){
              vect <- combi_df_sub[i,]
              combi_df_central_sp <- rbind(combi_df_central_sp, vect)
            }
          }

          best_combi <- combi_df_central_sp %>% arrange(desc(as.numeric(combi_df_central_sp[[nb_combi+1]]))) %>% ## sort values
            slice(1:input$nb_combi_display) ## keep the n best combinations to display
        }

        names_list <- c()
        for (i in 1:length(best_combi[,1])){
          combi_name <- c()
          for (j in 1:(length(best_combi[1,])-1)){
            if (best_combi[[j]][i] != "None"){
              combi_name <- paste(combi_name,  best_combi[[j]][i], sep = "\n")
            }
          }
          names_list <- c(names_list, combi_name)
        }

        best_combi<- cbind(best_combi, names_list)

        colnames(best_combi)[nb_combi+1] <- c("Intersection_volume")
        colnames(best_combi)[nb_combi+2] <- c("names_list")

        ggplot(data = best_combi, aes(x = reorder(names_list,-Intersection_volume) , y = Intersection_volume)) +
          geom_bar(stat="identity")+
          xlab("combinations")


      })

      output$plot2 <- renderPlot({

        species_abiotics_df_sub <- species_abiotics_df %>% filter(
          species %in% input$species_show
        )

        ggplot(species_abiotics_df_sub,
               aes(x = species_abiotics_df_sub[,input$Factor],
                   fill = species)) +
          geom_density(alpha = 0.4) +
          xlab(paste0(as.character(input$Factor)))

      })

    }
  )
}

