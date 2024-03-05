#' Mixfish_projections UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom datamods select_group_server select_group_ui
#' @importFrom mixfishtools plot_catchScenStk
mod_Mixfish_projections_ui <- function(id){
  ns <- NS(id)
  tagList(
    select_group_ui(
      id = ns("my-filters"),
      params = list(
        scenario = list(inputId = "scenario", "Management Scenario:"),
        stock = list(inputId = "stock", "Fish Stock"))
      
    ),
    plotOutput(ns("headline_bars"), height = "85vh")
 
  )
}
    
#' Mixfish_projections Server Functions
#'
#' @noRd 
mod_Mixfish_projections_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    ####### headline bar plot
    data_reactive <- reactive({
      catchScenarioStk
      
    })
    
    data_filter_module <- select_group_server(
      id = "my-filters",
      data = data_reactive(),
      vars = reactive(c("scenario", "stock"))
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
      
      plot_catchScenStk(data =  data_filter_module(), adv = catchRange)
      
      
    })
  })
}
    
## To be copied in the UI
# mod_Mixfish_projections_ui("Mixfish_projections_1")
    
## To be copied in the server
# mod_Mixfish_projections_server("Mixfish_projections_1")
