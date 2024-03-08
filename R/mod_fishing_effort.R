#' fishing_effort UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom plotly plotlyOutput
mod_fishing_effort_ui <- function(id){
  ns <- NS(id)
  tagList(
    navset_card_tab(full_screen = TRUE,
  
      select_group_ui(
        id = ns("effort-filters"),
        params = list(
          fleet = list(inputId = "fleet", "Fleet"),
          stock = list(inputId = "stock", "Fish Stock"))),
      nav_panel("Changes in fishing effort",
          card(fill = F,
               bslib::layout_columns(col_widths = c(6,6),
                  radioButtons(ns("scenario"), "Select comparison scenario", choices = c("min", "max", "cod-ns"), selected = "min", inline = T),
                  radioButtons(ns("year"), "Select advice year", choices = c(2020, 2021, 2022), selected = 2022, inline = T))),
          card(fill = T, height = "70vh", full_screen = T,
              plotly::plotlyOutput(ns("relative_effort_fleet_stock_plot"), height = "100%"))
            ),
      nav_panel("Limiting stocks",
              card( fill = T, height = "70vh",
                plotly::plotlyOutput(ns("fleet_stock_plot"), height = "100%"))
                )
            
              
        )
      )
}
    
#' fishing_effort Server Functions
#'
#' @noRd 
#' @importFrom ggplot2 theme element_text coord_cartesian
#' @importFrom plotly ggplotly renderPlotly layout
#' @importFrom mixfishtools plot_relEffortFltStk plot_effortFltStk
#' 
mod_fishing_effort_server <- function(id, region){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    fleet_stock_reactive <- reactive({
    
      stfFltStkSum
    })
    
    fleet_reactive <- reactive({
      if(is.null(input$`effort-filters-fleet`)){
        stfFltSum
      } else {
        stfFltSum %>% dplyr::filter(fleet %in% input$`effort-filters-fleet`)
      }
    })
    
    effort_filter_module <- select_group_server(
      id = "effort-filters",
      data = fleet_stock_reactive(),
      vars = reactive(c("fleet", "stock")) 
    )
    
    
    output$relative_effort_fleet_stock_plot <- renderPlotly({
   
      validate(
        need(nrow(effort_filter_module()) >=1, message = "No data in subset"), 
        need(nrow(fleet_reactive()) >=1, message = "No data in subset")
      )
      df <- compute_fleet_stock_effort(input_df = effort_filter_module(), input_effort = fleet_reactive(), advice_year = input$year, scenario = input$scenario)
      
      plot <- plot_relEffortFltStk(data = df) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 0, vjust = 0.3),
              text = element_text(size = 13)) +
        coord_cartesian() 
      
      plot %>% ggplotly() %>% 
        layout(autosize = TRUE)
    })
    
    
    output$fleet_stock_plot <- renderPlotly({
      
      validate(
        need(nrow(effort_filter_module()) >=1, message = "No data in subset"), 
        need(nrow(fleet_reactive()) >=1, message = "No data in subset")
      )
      
      df <- compute_limiting_stocks(input_df = effort_filter_module(), input_effort = fleet_reactive(), advice_year = input$year)
      
      plot <- plot_effortFltStk(data = df, refTable = refTable) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 0, vjust = 0.3),
              text = element_text(size = 13)) +
        coord_cartesian() 
      
      plot %>% ggplotly() %>% 
        layout(autosize = TRUE)
    })
  })
}
    
## To be copied in the UI
# mod_fishing_effort_ui("fishing_effort_1")
    
## To be copied in the server
# mod_fishing_effort_server("fishing_effort_1")
