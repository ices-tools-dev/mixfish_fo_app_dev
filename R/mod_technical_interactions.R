#' technical_interactions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_technical_interactions_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' technical_interactions Server Functions
#'
#' @noRd 
mod_technical_interactions_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_technical_interactions_ui("technical_interactions_1")
    
## To be copied in the server
# mod_technical_interactions_server("technical_interactions_1")
