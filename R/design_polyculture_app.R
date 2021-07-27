#' Title App to design polyculture system
#'
#' @param rescaled_combi_df Data frame with rescaled compatibility index
#' @param species_abiotics_df dataframe with all abiotic variables
#'
#' @return
#' @import ggplot2
#'@importFrom dplyr arrange
#'@importFrom dplyr slice
#' @export
#'
#' @examples
design_polyculture_app <- function(rescaled_combi_df, species_abiotics_df){

  #get the list of parameters from the dataframe
  selected_abiotics <- as.list(colnames(species_abiotics_df[,-1]))

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

  #ask users for the max number of species in combinations
  nb_combi <- dlg_list(title = "Chose the max number of species in combinations", c(2:(length(hv_list@HVList))))$res
  nb_combi <- as.numeric(nb_combi)

  #get species list from the hypervolume list
  species_list_hv_selected <- c()
  for (i in 1:length(hv_list@HVList)){
    species_list_hv_selected <- c(species_list_hv_selected, hv_list[[i]]@Name)
  }


  #divide temperatures and pH by 10
  abiotics_df$annual_meanT <- abiotics_df$annual_meanT/10
  abiotics_df$maxT_WM <- abiotics_df$maxT_WM/10
  abiotics_df$annual_rangeT <- abiotics_df$annual_rangeT/10
  abiotics_df$ph_max <- abiotics_df$ph_max/10
  abiotics_df$minT_CM <- abiotics_df$minT_CM/10
  abiotics_df$meanT_DQ <- abiotics_df$meanT_DQ/10


  shinyApp(

    ui = fluidPage(

      titlePanel("Species compatibility"),

      sidebarLayout(

        sidebarPanel(

          #select central species
          selectInput(inputId = "central_species",
                      label = "Choose a central species:",
                      choices = c("None", species_list_hv_selected)),

          #select number of species in combinations
          selectInput(inputId = "nb_species",
                      label = "Choose a number of species:",
                      choices = c("All", 2:as.numeric(nb_combi))),

          #select number of combinations to show
          selectInput(inputId = "nb_combi_display",
                      label = "Choose the number of best combinations to display:",
                      choices = 5:50),

          #select abiotics for density diagram
          selectInput(inputId = "Factor",
                      label = "Choose a factor:",
                      choices = selected_abiotics),

          #select specis to show on the density diagram
          checkboxGroupInput(inputId = "species_show",
                             label = "Chose species to show:",
                             choiceNames = species_list_hv_selected,
                             choiceValues = species_list_hv_selected)

        ),


        # Main panel for displaying outputs ----
        mainPanel(
          # plot ranking
          plotOutput("plot1", width = "100%", height = 600),
          #plot density diagrams
          plotOutput("plot2", width = "100%", height = 400)
        )
      )

    ),



    #### set server #####
    server = function(input, output) {

      selected_species <- species_list_hv_selected


      #static background map
      output$plot1 <- renderPlot({

        #either you compare combinations with the same number of species (else, line 130), or all combinations (first case below)
        if (input$nb_species == "All") {

          #either you set a central species or not
          if(input$central_species == "None"){
            #sort the combinations by volume and retain only the "nb_combi_display" first
            best_combi <- rescaled_combi_df %>% arrange(desc(as.numeric(rescaled_combi_df[[nb_combi + 1]]))) %>%
              slice(1:input$nb_combi_display)
          }
          else {
            #keep only rows including the central species
            row_sub = apply(rescaled_combi_df, 1, function(row) all(row != input$central_species))
            combi_df_sub <- rescaled_combi_df[!row_sub,]

            best_combi <- combi_df_sub %>% arrange(desc(as.numeric(combi_df_sub[[nb_combi +1]]))) %>%
              slice(1:input$nb_combi_display)
          }
        }

        #same as before but with combinations of the same number of species
        else{
          if (input$central_species == "None"){
            combi_df_sp <- data.frame(matrix(ncol = as.numeric(input$nb_species), nrow = 0))
            for (i in 1:length(rescaled_combi_df[,1])){
              n <- rowSums(rescaled_combi_df == "None")
              if ((nb_combi - n[i]) == as.numeric(input$nb_species)){
                vect <- rescaled_combi_df[i,]
                combi_df_sp <- rbind(combi_df_sp, vect)
              }
            }
            best_combi <- combi_df_sp %>% arrange(desc(as.numeric(combi_df_sp[[nb_combi+1]]))) %>% ## sort by index
              slice(1:input$nb_combi_display) ## keep only best combinations

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

        }

        #set the name of the combinations to display
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

        #plot the barchart
        ggplot(data = best_combi, aes(x = reorder(names_list,-Intersection_volume) , y = Intersection_volume)) +
          geom_bar(stat="identity")+
          xlab("combinations")+
          geom_errorbar(aes(ymin=Intersection_volume-(Intersection_volume/10), ymax=Intersection_volume+(Intersection_volume/10)), width=.2,
                        position=position_dodge(.9))


      })

      #plot density diagram
      output$plot2 <- renderPlot({

        #subset
        species_abiotics_df_sub <- species_abiotics_df %>% filter(
          species %in% input$species_show
        )

        #plot
        ggplot(species_abiotics_df_sub,
               aes(x = species_abiotics_df_sub[,input$Factor],
                   fill = species)) +
          geom_density(alpha = 0.4) +
          xlab(names(selected_abiotics[which( selected_abiotics %in% input$Factor )]))

      })

    }
  )
}

