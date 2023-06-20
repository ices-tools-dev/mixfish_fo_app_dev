
# msg <- function(...) {
#   emph <- "\n****************\n"
#   cat(emph, ..., emph)
# }


# # required if using most recent version of sf
# sf::sf_use_s2(FALSE)

# options(icesSAG.use_token = FALSE)

allEco <- fread("Data/fdi_ecoregion/all_ecoregions.csv")

source("plot_fleet_stock_effort.R")

############# Start server function ################

server <- function(input, output, session) {
  msg("server loop start:\n  ", getwd())
  map_panel_server(input, output, session)

  observeEvent(input$web_button, {
    updateNavbarPage(session, "tabset", selected = "FO")
  })
  observeEvent(input$web_button2, {
    updateNavbarPage(session, "tabset", selected = "MixFish")
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

  output$headline_bars <- renderPlotly({

    # head(catchScenStk)
    ggplotly(plot_catchScenStk(data = res_mod()))
  })

  #### fdi landings map
  # lutPal <- rbind(
  #   data.frame(pal = "spectral", fun = "brewer.spectral"),
  #   data.frame(pal = "paired", fun = "brewer.paired"),
  #   data.frame(pal = "cols25", fun = "cols25"),
  #   data.frame(pal = "set1", fun = "brewer.set1"),
  #   data.frame(pal = "set2", fun = "brewer.set2"),
  #   data.frame(pal = "set3", fun = "brewer.set3"),
  #   data.frame(pal = "alphabet", fun = "alphabet"),
  #   data.frame(pal = "alphabet2", fun = "alphabet2"),
  #   data.frame(pal = "polychrome", fun = "polychrome"),
  #   data.frame(pal = "glasbey", fun = "glasbey")
  # )

  data_reactive_landings <- reactive({
    req(input$selected_locations)
    print(input$selected_locations)
    
    dataEco <- allEco %>% dplyr::filter(ecoregion == input$selected_locations) %>% mesh_size_extractor(.)
  })

  # imported_data <- eventReactive(req(input$selected_locations),{
  #   # req(input$selected_locations)
  #   print(input$selected_locations)
  #   # filename <- paste0("Data/fdi_ecoregion/", input$selected_locations, "_data.Rdata")
  #   filename <- "Data/fdi_ecoregion/ecoRegions_data.csv"
    
  #   data <- fread(filename)
  #   # data <- load(filename)
  #   # 
  #   print(unique(data$gear_type))
  # })

  # updateSelectizeInput(
  #       inputId = "gear_type", 
  #       label = "Gear type:",
  #       selected = c("OTB"),
  #       choices = sort(unique(imported_data$gear_type))
       
  # )

  # updateSelectizeInput(
  #       inputId = "species",
  #       label = "Species:", 
  #       selected = c("COD", "HAD", "POK"),
  #       choices = sort(unique(imported_data$species))
        
  # )
  # filter data ----
#   filtered_data <- reactive({
#     req(input$selected_locations)
#     print(input$selected_locations)
#     filename <- paste0("Data/fdi_ecoregion/", input$selected_locations, "_data.Rdata")
#     # # filename <- paste0("Data/fdi_ecoregion/", input$selected_locations, "_data.csv")
    
#     # # data <- fread(filename)
#     load(filename)
#     print(unique(data$gear_type))
#     #  data <- mesh_size_extractor(data)

#     # dfsub <- subset(data, species %in% input$species & 
#     #   gear_type %in% input$gear_type &
#     #   vessel_length %in% input$vessel_length &  
#     #   mesh_size_min >= input$mesh_range[1] &
#     #   mesh_size_max <= input$mesh_range[2] &
#     #   year >= input$year_range[1] &
#     #   year <= input$year_range[2])
#     # print(dfsub)
# })

  res_mod2 <- callModule(
    module = selectizeGroupServer,
    id = "fdi_filters",
    data = data_reactive_landings(),
    vars = c(
      "species", "gear_type", "ecoregion"
    )
  )

  data_effort_reactive <- reactive({
    
    fleet_stock_sum <- read.table("Data/stfFltStkSum.csv")
    fleet_sum <- read.table("Data/stfFltSum.csv")
    
    list(fleet_stock_sum = fleet_stock_sum, 
         fleet_sum = fleet_sum)
  })
  
output$fleet_stock_effort_plot <- renderPlotly({
  plot_fleet_stock_effort(df = data_effort_reactive()$fleet_stock_sum, 
                                                          eff = data_effort_reactive()$fleet_sum)
})
  #   data_effort_reactive <- reactive({
#     stfFltStkSum <- read.table("Data/stfFltStkSum.csv")
#     stfFltSum <- read.table("Data/stfFltSum.csv")
#     advYr <- 2022 # advice year
# df <- subset(stfFltStkSum, scenario == "min" & year == advYr)
# eff <- subset(
#   stfFltSum, scenario == "min" & year == advYr)[,c("fleet", "effort")]
# sqEff <- subset(
#   stfFltSum, scenario == "sq_E" & year == advYr)[,c("fleet", "effort")]
# names(sqEff)[2] <- "sqEffort"
# eff <- merge(x = eff, y = sqEff, all.x = TRUE)
# df <- merge(x = df, y = eff, all.x = TRUE)
# df$quotaEffort <- df$effort / df$quotaUpt
# df$relEffort <- df$quotaEffort / df$sqEffort
# df$scenario <- df$stock
# restr.stks <- c("COD-NS", "HAD", "PLE-EC", "PLE-NS", "POK", "SOL-EC",
#   "SOL-NS", "TUR", "WHG-NS", "WIT", "NEP6", "NEP7", "NEP8", "NEP9")
# df <- subset(df, stock %in% restr.stks)
# #'
# # convert to percentage change
# df$var <- 100*(df$relEffort-1)
# #'
# # optional upper limit (e.g. 100)
# df$var <- ifelse(df$var > 100, 100, df$var)
#'
# plot


#   })

# output$effort_plot <- renderPlotly({

#     # head(catchScenStk)
#     ggplotly(plot_relEffortFltStk(data = data_effort_reactive()))
#   })


}
