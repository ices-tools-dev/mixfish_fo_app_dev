#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shiny::navbarPage(
        position = "static-top",
        collapsible = TRUE,
        # tab title
        title = "MIXFISH",
        id = "tabset",
        fluid = TRUE,
        
      tabPanel(title = "Region Selection",
               mod_map_selector_ui("map_selector_1")
      ),
      tabPanel(title = "Fishing Effort",
      mod_fishing_effort_ui("fishing_effort_1"))
 
    
  )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "mixfishXplorer"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
