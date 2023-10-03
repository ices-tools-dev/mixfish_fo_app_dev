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
    textOutput(ns("selected_1")),
    plotly::plotlyOutput(ns("fleet_stock_effort_plot"), height = "100%", width = "50%")
 
  )
}
    
#' fishing_effort Server Functions
#'
#' @noRd 
#' @importFrom plotly ggplotly renderPlotly
mod_fishing_effort_server <- function(id, region){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    data_effort_reactive <- reactive({
    
      list(fleet_stock_sum = fleet_stock_sum, 
           fleet_sum = fleet_sum)
    })
    
    output$fleet_stock_effort_plot <- renderPlotly({
      
      df <- compute_fleet_stock_effort(df = data_effort_reactive()$fleet_stock_sum, eff = data_effort_reactive()$fleet_sum)
      mixfishtools::plot_relEffortFltStk(data = df) %>% ggplotly()
    })
    
    
    
    # advice_year<- 2022 # advice year
    # reactive_effort_by_fleet <- reactive({
    #   
    #   # fleet_stock_sum <- read.table("Data/stfFltStkSum.csv")
    #   # fleet_sum <- read.table("Data/stfFltSum.csv")
    #   # refTable <- read.table("Data/refTable.csv")
    #   
    #   effort_by_fleet(fleet_stock_sum, fleet_sum, refTable, advYr = advice_year)
    #   
    # })
    # 
    # output$plot_effort_by_fleet <- renderPlotly({
    #   ggplotly(plot_effortFltStk_edit(data = reactive_effort_by_fleet(), refTable = reactive_refTable()) )  
    #   
    # })
    
  })
}
    
## To be copied in the UI
# mod_fishing_effort_ui("fishing_effort_1")
    
## To be copied in the server
# mod_fishing_effort_server("fishing_effort_1")
