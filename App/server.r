
# msg <- function(...) {
#   emph <- "\n****************\n"
#   cat(emph, ..., emph)
# }


# # required if using most recent version of sf
# sf::sf_use_s2(FALSE)

# options(icesSAG.use_token = FALSE)

data <- fread("Data/fdi_ecoregion/all_ecoregions.csv")



############# Start server function ################

server <- function(input, output, session) {
  msg("server loop start:\n  ", getwd())


  map_panel_server(input, output, session)

  observeEvent(input$selected_locations,{
  
  
  observeEvent(input$web_button, {
    updateNavbarPage(session, "tabset", selected = "FO")    
  })
  observeEvent(input$web_button2, {
    updateNavbarPage(session, "tabset", selected = "MixFishConsiderations")
  })
  observeEvent(input$web_button3, {
    updateNavbarPage(session, "tabset", selected = "MixFishAdvice")
  })


  

  lutPal <- rbind(
      data.frame(pal = "spectral", fun = "brewer.spectral"),
      data.frame(pal = "paired", fun = "brewer.paired"),
      data.frame(pal = "cols25", fun = "cols25"),
      data.frame(pal = "set1", fun = "brewer.set1"),
      data.frame(pal = "set2", fun = "brewer.set2"),
      data.frame(pal = "set3", fun = "brewer.set3")
    )

  filtered_data <- eventReactive(req(input$filter_data), {
    req(input$selected_locations)
    # print(input$selected_locations)
    dataEco <- data %>%
      dplyr::filter(ecoregion == input$selected_locations) %>%
      mesh_size_extractor(.)

    dfsub <- subset(dataEco, species %in% input$species & 
      gear_type %in% input$gear_type &
      vessel_length %in% input$vessel_length &  
      mesh_size_min >= input$mesh_range[1] &
      mesh_size_max <= input$mesh_range[2] &
      year >= input$year_range[1] &
      year <= input$year_range[2])
    
  })

  # updateSelectInput(
  #                               inputId = "species", 
  #                               label = "Species:",
  #                               selected = NULL,
  #                               choices = sort(unique(filtered_data()$species))
                               
  #                           )

  # updateSelectInput(
  #                               inputId = "gear_type", 
  #                               label = "Gear type:",
  #                               selected = NULL,
  #                               choices = sort(unique(filtered_data()$gear_type))
                                
  #                           )
  
  # species aggregation ----
  filtered_data_species <- reactive({
    
    dfsub <- filtered_data()
    lutCol <- data.frame(species = sort(unique(filtered_data()$species)))
    lutCol$col <- do.call(lutPal$fun[match(input$pal, lutPal$pal)],
      args = list(n = length(lutCol$species)))
    
    agg1 <- aggregate(totwghtlandg ~ icesname + species, data = dfsub, FUN = sum, na.rm = T)
    names(agg1)[3] <- "landings"
    agg1 <- subset(agg1, landings > 0) # remove zero landings
    agg2 <- aggregate(landings ~ icesname, data = agg1, FUN = sum, na.rm = T)
    names(agg2)[2] <- "sumLandings"
    
    agg <- merge(agg1, agg2, all.x = T)
    agg$percLandings <- agg$landings / agg$sumLandings
    agg$sc <- sqrt(agg$sumLandings/max(agg$sumLandings, na.rm = T))
    agg <- cbind(agg, ices.rect(rectangle = agg$icesname))
    agg$col <- lutCol$col[match(agg$species, lutCol$species)]

    agg
  })
  
  # gear type aggregation ----
  filtered_data_gear_type <- reactive({
    
    dfsub <- filtered_data()
   
    lutCol <- data.frame(gear_type = sort(unique(filtered_data()$gear_type)))
    palet <- match(input$pal, lutPal$pal)
    if (length(lutCol$gear_type)<3) {
      lutCol$col <- do.call(lutPal$fun[palet],
      args = list(n = 3))[1:length(lutCol$gear_type)]
    } else {
      lutCol$col <- do.call(lutPal$fun[palet],
      args = list(n = length(lutCol$gear_type)))
      }
    
    agg1 <- aggregate(totwghtlandg ~ icesname + gear_type, data = dfsub, FUN = sum, na.rm = T)
    names(agg1)[3] <- "landings"
    agg1 <- subset(agg1, landings > 0) # remove zero landings
    agg2 <- aggregate(landings ~ icesname, data = agg1, FUN = sum, na.rm = T)
    names(agg2)[2] <- "sumLandings"
    
    agg <- merge(agg1, agg2, all.x = T)
    agg$percLandings <- agg$landings / agg$sumLandings
    agg$sc <- sqrt(agg$sumLandings/max(agg$sumLandings, na.rm = T))
    agg <- cbind(agg, ices.rect(rectangle = agg$icesname))
    agg$col <- lutCol$col[match(agg$gear_type, lutCol$gear_type)]
    agg
    
  })

  output$text <- renderUI({
    req(input$selected_locations)
    HTML(paste0("The ecoregion selected is ", input$selected_locations))
    # print(head(filtered_data()) )
  })





  # Render map(s)
  output$map_species <- renderPlot({
    plot_map_species(filtered_data_species(), input$sc, input$selected_locations)
    
  })
  
  output$map_gear_type <- renderPlot({
    plot_map_gear_type(filtered_data_gear_type(), input$sc, input$selected_locations)
    
  })
  
  # Render corr plot
  output$corr_species <- renderPlot({
    plot_corr_species(filtered_data_species())
  })
  
  output$corr_gear_type <- renderPlot({
    plot_corr_gear_type(filtered_data_gear_type())
  })

  data_effort_reactive <- reactive({
      
      fleet_stock_sum <- read.table("Data/stfFltStkSum.csv")
      fleet_sum <- read.table("Data/stfFltSum.csv")
      
      list(fleet_stock_sum = fleet_stock_sum, 
          fleet_sum = fleet_sum)
    })
    
  output$fleet_stock_effort_plot <- renderPlotly({
    plot_fleet_stock_effort(df = data_effort_reactive()$fleet_stock_sum, eff = data_effort_reactive()$fleet_sum)



})

####### headline bar plot
  data_reactive <- reactive({
    read.table("Data/catchScenStk.csv")
    
  })
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = data_reactive(),
    vars = c(
      "scenario", "stock"
    )
  )

  output$headline_bars <- renderPlot({
    catchRange <- rbind(
                data.frame(stock = "COD-NS", advice = 14276, lower = 9701, upper = 14276),
                data.frame(stock = "HAD", advice = 128708, lower = 111702, upper = 128708),
                data.frame(stock = "PLE-EC", advice = 6365, lower = 4594, upper = 6365),
                data.frame(stock = "PLE-NS", advice = 142507, lower = 101854, upper = 195622),
                data.frame(stock = "POK", advice = 49614, lower = 30204, upper = 49614),
                data.frame(stock = "SOL-EC", advice = 1810, lower = 1068, upper = 2069),
                data.frame(stock = "SOL-NS", advice = 15330, lower = 9523, upper = 21805),
                data.frame(stock = "TUR", advice = 3609, lower = 2634, upper = 4564),
                data.frame(stock = "WHG-NS", advice = 88426, lower = 70169, upper = 91703),
                data.frame(stock = "WIT", advice = 1206, lower = 875, upper = 1206)
)
    
    plot_catchScenStk(data = res_mod(), adv = catchRange)


  })




reactive_refTable <- reactive({
    read.table("Data/refTable.csv")
    
  })
reactive_effort_by_fleet <- reactive({
    fleet_stock_sum <- read.table("Data/stfFltStkSum.csv")
    fleet_sum <- read.table("Data/stfFltSum.csv")
    refTable <- read.table("Data/refTable.csv")
    
    effort_by_fleet(fleet_stock_sum, fleet_sum, refTable)
    
  })

  # res_mod <- callModule(
  #   module = selectizeGroupServer,
  #   id = "my-filters",
  #   data = data_reactive(),
  #   vars = c(
  #     "scenario", "stock"
  #   )
  # )

  output$plot_effort_by_fleet <- renderPlotly({
    ggplotly(plot_effortFltStk_edit(data = reactive_effort_by_fleet(), refTable = reactive_refTable()) )  
    # p <- p + theme(text = element_text(size = 12))    
  })


})

}
