#' fishing_effort UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom plotly plotlyOutput
mod_fishing_effort_ui <- function(id){
  ns <- NS(id)
  tagList(
    card(full_screen = T, fill = T, height = "90vh",
      plotly::plotlyOutput(ns("fleet_stock_effort_plot"), height = "100%")
         )
    )
}
    
#' fishing_effort Server Functions
#'
#' @noRd 
#' @importFrom plotly ggplotly renderPlotly layout
#' @importFrom mixfishtools plot_relEffortFltStk
#' 
mod_fishing_effort_server <- function(id, region){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    data_effort_reactive <- reactive({
    
      list(fleet_stock_sum = fleet_stock_sum, 
           fleet_sum = fleet_sum)
    })
    
    cdata <- session$clientData
    output$fleet_stock_effort_plot <- renderPlotly({
      
      df <- compute_fleet_stock_effort(df = data_effort_reactive()$fleet_stock_sum, eff = data_effort_reactive()$fleet_sum)
      plot_relEffortFltStk(data = df) %>% 
        ggplotly() %>% 
        layout(autosize = TRUE)
    })
  })
}
    
## To be copied in the UI
# mod_fishing_effort_ui("fishing_effort_1")
    
## To be copied in the server
# mod_fishing_effort_server("fishing_effort_1")
