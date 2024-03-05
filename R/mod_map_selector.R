#' map_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_map_selector_ui <- function(id){
  ns <- NS(id)
  tagList(
    
        leaflet::leafletOutput(ns("map")), 
        selectizeInput(
          inputId = ns("selected_locations"),
          label = "Case study regions",
          choices = sort(eco_shape$Ecoregion),
          selected = "Greater North Sea",
          multiple = FALSE,
          width = "100%",
          options = list(placeholder = "Select Ecoregion(s)"))
  )
}
    
#' map_selector Server Functions
#'
#' @noRd 
mod_map_selector_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$map <- leaflet::renderLeaflet({
      map_ecoregion(eco_shape, map_shape)
    })
    
    proxy_map <- leaflet::leafletProxy("map")
    
    # create empty vector to hold all click ids
    selected_1 <- reactiveValues(groups = vector())
    
    # find index
    observeEvent(input$map_shape_click, {
      
      if (input$map_shape_click$group == "Eco_regions") {
        selected_1$groups <- c(selected_1$groups, input$map_shape_click$id)
        
        proxy_map %>%
          leaflet::showGroup(group = input$map_shape_click$id)
      }
      
      updateSelectizeInput(session,
                           inputId = "selected_locations",
                           choices = eco_shape$Ecoregion,
                           selected = selected_1$groups)
    })
    
    
    observeEvent(input$selected_locations,
                 {
                   removed_via_selectInput <- setdiff(selected_1$groups, input$selected_locations)
                   added_via_selectInput <- setdiff(input$selected_locations, selected_1$groups)
                   
                   if (length(removed_via_selectInput) > 0) {
                     selected_1$groups <- input$selected_locations
                     
                     proxy_map %>% leaflet::hideGroup(group = removed_via_selectInput)
                   }
                   
                   if (length(added_via_selectInput) > 0) {
                     selected_1$groups <- input$selected_locations
                     
                     proxy_map %>% leaflet::showGroup(group = added_via_selectInput)
                     
                   }
                 },
                 ignoreNULL = FALSE
                )
    return(reactive(selected_1$groups))
  })
}

    
## To be copied in the UI
# mod_map_selector_ui("map_selector_1")
    
## To be copied in the server
# mod_map_selector_server("map_selector_1")
