#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib value_box card card_header card_body layout_column_wrap bs_theme layout_sidebar navset_card_tab nav_panel
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shiny::navbarPage(
        theme = bs_theme(bootswatch = "flatly"),
        position = "static-top",
        collapsible = TRUE,
        # tab title
        id = "tabset",
        fluid = TRUE,
        title = "MIXFISH",
        header = div(style = "padding-top: 15px;", selectInput(inputId = "regionSelect", label = NULL, selected = "Irish Sea", 
                             choices = c("Irish Sea", "Greater North Sea", "Celtic Seas", "Baltic Sea", "Bay of Biscay and the Iberian Coast"))),
      tabPanel(title = "Projections",
               mod_Mixfish_projections_ui("Mixfish_projections_1")),
      tabPanel(title = "Fishing Effort",
               mod_fishing_effort_ui("fishing_effort_1")),
      tabPanel(title = "Metier and Fleet Definition",
                mod_metier_fleet_sankey_ui("metier_fleet_sankey_1")
               ),
      tabPanel(title = "Technical Interactions",
                mod_technical_interactions_ui("technical_interactions_1")
               ),
      tabPanel(title = "Spatial Landings",
                mod_spatial_landings_ui("spatial_landings_1")
               )
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
