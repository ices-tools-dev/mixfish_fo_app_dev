#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_map_selector_server("map_selector_1")
  mod_fishing_effort_server("fishing_effort_1", region = selected_1)
  mod_technical_interactions_server("technical_interactions_1")
  mod_metier_fleet_sankey_server("metier_fleet_sankey_1")
  mod_spatial_landings_server("spatial_landings_1")
  
}
