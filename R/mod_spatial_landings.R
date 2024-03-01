#' spatial_landings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinycssloaders withSpinner
mod_spatial_landings_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        sliderInput(
          inputId = ns("year_range"),
          label = "Year range:",
          step = 1,
          sep = "",
          min = 2013,
          max = 2021,
          value = c(2018, 2021)
        ),
        selectInput(
          inputId = ns("vessel_length"), label = "Vessel Length:",
          selected = c("VL1218", "VL1824"),
          choices = list(
            "0-12m" = "VL0010",
            "10-12m" = "VL1012",
            "12-18m" = "VL1218",
            "18-24m" = "VL1824",
            "24-40m" = "VL2440",
            "40m+" = "VL40XX",
            "Not known" = "NK"
          ),
          multiple = TRUE
        ),
        selectInput(
          inputId = ns("gear_type"), label = "Gear type:",
          choices = NULL,
          multiple = TRUE
        ),
        sliderInput(inputId = ns("mesh_range"), "Mesh range [mm]:",
                    step = 10, sep = "",
                    min = 0, max = 250,
                    value = c(0, 250)
        ),
        selectInput(
          inputId = ns("species"), label = "Species:",
          choices = NULL,
          multiple = TRUE
        ),
        selectInput(
          inputId = ns("pal"), label = "Palette:",
          selected = "spectral",
          choices = c("spectral", "paired", "cols25", "set1", "set2", "set3"),
          multiple = FALSE
        ),
        checkboxInput(inputId = ns("sc"), label = "Scaled landings", value = TRUE),
        actionButton(inputId = ns("filter_data"), label = "Update Filter", width = "100%")
        # selectizeGroupUI(
        #     id = "fdi_filters",
        #     params = list(
        #         species = list(inputId = "species", title = "Species:"),
        #         gear_type = list(inputId = "gear_type", title = "Gear type:"),
        #         ecoregion = list(inputId = "ecoregion", title = "ecoregion:")
        #     ),
        #     inline = TRUE
        # )
      ),
      mainPanel(
        width = 9,
        htmlOutput(ns("text")),
        fluidRow(
          column(5, shinycssloaders::withSpinner(plotOutput(ns("map_species"), width = "100%", height = 600), type = 1, color = "#0275D8")),
          column(5, shinycssloaders::withSpinner(plotOutput(ns("map_gear_type"), width = "100%", height = 600), type = 1, color = "#0275D8"))
        ),
        fluidRow(
          column(5, withSpinner(plotOutput(ns("corr_species"), width = "100%", height = 400), type = 1, color = "#0275D8")),
          column(5, withSpinner(plotOutput(ns("corr_gear_type"), width = "100%", height = 400), type = 1, color = "#0275D8"))
        )
      )
    )
  )
}
    
#' spatial_landings Server Functions
#'
#' @noRd 
#' @importFrom mapplots ices.rect
mod_spatial_landings_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    loc <- "Greater North Sea"
    observe({
      
      # loc <- input$selected_locations
      
      # Filter data based on selected region
      filtered_data <- fdi_data[fdi_data$ecoregion == loc, ]
      
      # Get unique values
      unique_gear <- unique(filtered_data$gear_type)
      unique_species <- unique(filtered_data$species)
      
      # Update the second dropdown choices
      updateSelectInput(session, "gear_type", choices = unique_gear, selected = unique_gear[1])
      updateSelectInput(session, "species", choices = unique_species, selected = unique_species[1])
    })
    
    
    filtered_data <- eventReactive(req(input$filter_data), {
      validate(
        need(sum(sapply(list(input$gear_type, input$vessel_length, input$species), FUN = is.null)) ==0, message = "Please select input values for all parameters")
      )
      #req(input$selected_locations)
    
      dataEco <- fdi_data %>%
        dplyr::filter(ecoregion == loc) %>%
        mesh_size_extractor(.)
      
      
      
      dfsub <- subset(dataEco, species %in% input$species & 
                        gear_type %in% input$gear_type &
                        vessel_length %in% input$vessel_length &  
                        mesh_size_min >= input$mesh_range[1] &
                        mesh_size_max <= input$mesh_range[2] &
                        year >= input$year_range[1] &
                        year <= input$year_range[2])
      
    })
    
   
    # species aggregation ----
    filtered_data_species <- reactive({
      
      dfsub <- filtered_data()
      lutCol <- data.frame(species = sort(unique(filtered_data()$species)))
      lutCol$col <- get_palette_colours(palette = input$pal, plot_variable = filtered_data()$species)

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
      
      lutCol$col <- get_palette_colours(palette = input$pal, plot_variable = filtered_data()$gear_type)
      
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
    
    # Render map(s)
    output$map_species <- renderPlot({
      # plot_map_species(filtered_data_species(), input$sc, input$selected_locations)
      plot_map_species(filtered_data_species(), input$sc, loc)
      
    })
    
    output$map_gear_type <- renderPlot({
      # plot_map_gear_type(filtered_data_gear_type(), input$sc, input$selected_locations)
      plot_map_gear_type(filtered_data_gear_type(), input$sc, loc)
      
    })
    
    # Render corr plot
    output$corr_species <- renderPlot({
      req(length(input$species) >=2)
      
      plot_corr_species(filtered_data_species())
    }) %>% bindEvent(input$filter_data)
    
    output$corr_gear_type <- renderPlot({
      req(length(input$gear_type) >=2)
      
      plot_corr_gear_type(filtered_data_gear_type())
    }) %>% bindEvent(input$filter_data)
    
  })
}
    
## To be copied in the UI
# mod_spatial_landings_ui("spatial_landings_1")
    
## To be copied in the server
# mod_spatial_landings_server("spatial_landings_1")
