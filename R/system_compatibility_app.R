#' Title App to test if the user's system is suitable to a species or combination
#'
#' @param hv_list a list of hypervolumes
#' @param abiotics_df data frame with abiotics data
#' @param user_param_syst user parameters
#' @param user_param_rescaled user parameters rescaled
#'
#'@importFrom data.table transpose
#' @return
#' @export
#'
#' @examples
system_compatibility_app <- function(hv_list, abiotics_df, user_param_syst, user_param_rescaled, species_abiotics_df, species_list){


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

#############################################################################

  #test system inclusion in species or combination hypervolume
  possible_combi <- c()
  for (i in 1:length(hv_list@HVList)){

    #check if the user_param_rescaled is a single line vector or a multilines dataframe
    if (length(as.data.frame(user_param_rescaled)[1,])==1){
      #if its a vector, it is transposed
      test <- hypervolume_inclusion_test(hv_list[[i]], points=transpose(as.data.frame(user_param_rescaled)), fast.or.accurate = "accurate")
    }

    else{
      test <- hypervolume_inclusion_test(hv_list[[i]], points=as.data.frame(user_param_rescaled), fast.or.accurate = "accurate")
      if ('FALSE' %in% test ){  #if any points is not included in the hypervolume, set the compatibility as false
        test <- FALSE
      }
      else{
        test <- TRUE
      }
    }

    if (test == TRUE){
      possible_combi <- c(possible_combi, hv_list[[i]]@Name)
    }

  }

  if (length(possible_combi)==0){
    stop("No species or combination can be reared in the system")
  }

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

  #divide temperatures and pH by 10
  abiotics_df$annual_meanT <- abiotics_df$annual_meanT/10
  abiotics_df$maxT_WM <- abiotics_df$maxT_WM/10
  abiotics_df$annual_rangeT <- abiotics_df$annual_rangeT/10
  abiotics_df$ph_max <- abiotics_df$ph_max/10
  abiotics_df$minT_CM <- abiotics_df$minT_CM/10
  abiotics_df$meanT_DQ <- abiotics_df$meanT_DQ/10


  shinyApp(ui = fluidPage(
    titlePanel("Species density"),
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

      mainPanel(
        plotOutput("plot", width = "100%", height = 400),
        verbatimTextOutput("txt1"),
        verbatimTextOutput("txt2")
      )
    )
  ),

  server = function(input, output) {

    output$txt1 <- renderText("Most probably suitable species or combinations :", sep = "\n")
    output$txt2 <- renderText(possible_combi, sep = "\n")

    output$plot <- renderPlot({
      species_abiotics_df_sub <- species_abiotics_df %>%
        filter(species %in% input$species_show)
      ggplot(species_abiotics_df_sub, aes(x = species_abiotics_df_sub[,input$Factor], fill = species))+
        geom_density(alpha = 0.4) +
        xlab(names(selected_abiotics[which(selected_abiotics %in%
                                             input$Factor)]))+
        geom_vline(xintercept =  user_param_syst[,input$Factor], alpha = 0.4)
    })
  }


  )

}
