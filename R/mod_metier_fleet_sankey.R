#' metier_fleet_sankey UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_metier_fleet_sankey_ui <- function(id){
  ns <- NS(id)
  tagList(
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::radioButtons(inputId = ns('sankey_selection'),
                              label = "Select Metier-to-Metier or Metier-to-Fleet",
                              choices = c("Metier-to-metier" = "metier", "Metier-to-fleet" = "flt", "Metier-to-Metier-to-Fleet" = "metmetflt",
                              selected =  "flt")
        ),
        shiny::mainPanel(ns("sankey"))
      )
  )
}
    
#' metier_fleet_sankey Server Functions
#'
#' @noRd 
mod_metier_fleet_sankey_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    output$sankey <- renderPlot({
      if (input$sankey_selection == "metier") {
    plot <- mixfishtools::plot_MetierSankey(Data = accessions) 
  } else {
    plot <- mixfishtools::plot_Metier_2_fleet_Sankey(Data = accessions)
      
  }
    }

    )
  })
}
    
## To be copied in the UI
# mod_metier_fleet_sankey_ui("metier_fleet_sankey_1")
    
## To be copied in the server
# mod_metier_fleet_sankey_server("metier_fleet_sankey_1")
