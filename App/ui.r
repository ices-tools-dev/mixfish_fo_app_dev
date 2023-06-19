############# Libraries ############
library(data.table)
library(dplyr)
library(dygraphs)
library(DT)
library(fisheryO)
library(htmltools)
library(htmlwidgets)
library(ggplot2)
library(ggradar)
library(ggtext)
library(glue)
library(gsubfn)
library(icesFO)
library(icesSAG)
library(icesTAF)
library(icesVocab)
library(leaflet)
library(plotly)
library(reshape2)
library(rintrojs)
library(RColorBrewer)
library(RCurl)
library(rvest)
library(scales)
library(sf)
library(shinyalert)
library(shinyBS)
library(shinycssloaders)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(stringr)
library(tidyr)
library(tidyverse)
library(tm)
library(widgetframe)
library(icesASD)
library(mixfishtools)
library(shiny)
library(shinyWidgets)
library(mapplots)
library(maps)
library(mapdata)
library(pals)
library(leaflet)
library(sf)
library(shinyjs)
library(reshape2)
library(psych)






########## Load utilities ############
source("utilities_load_ecoregion_shp.r")
source("utilities_ecoregion_mapping.r")
source("imageDimnames.r")
source("plotCor.r")
source("mesh_size_extractor.r")


# Load data

# load("Data/fdi_ecoregion/Greater North Sea_data.Rdata")



title_html <- tags$a(
    href = "https://github.com/ices-tools-dev/mixfishtools",
    tags$img(
        src = "hexSticker_mixfishtools_trans.png",
        style = "margin-top: -10px; padding-right:10px;padding-bottom:10px",
        height = "50px"
    )
)
tagList(
    useShinyjs(),
    introjsUI(),
    tags$script(src = "https://kit.fontawesome.com/ac71e9cf8e.js"),
    
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),





    navbarPage(
        position = "static-top",
        collapsible = TRUE,
        # tab title
        windowTitle = "mixfishtools",
        id = "tabset",
        fluid = TRUE,
        # navbar title
        title = title_html,
        tabPanel(
            "Ecoregion selection",
            sidebarLayout(
                sidebarPanel(
                    width = 7,
                    leafletOutput("map_ecoregion"), 
                    selectizeInput(
                            inputId = "selected_locations",
                            label = "ICES Ecoregions",
                            choices = sort(shape_eco$Ecoregion),
                            selected = "Greater North Sea",
                            multiple = FALSE,
                            width = "100%",
                            options = list(
                            placeholder = "Select Ecoregion(s)"
                            )
                        )
                ),
                mainPanel(
                    width = 5,
                    fluidRow(
                        tags$button(
                            id = "web_button",
                            class = "btn action-button",
                            align = "center",
                            tags$img(
                                src = "FishO.png",
                                height = "200px"
                            )
                        ),
                        br(),
                        br(),
                        tags$button(
                            id = "web_button2",
                            class = "btn action-button",
                            align = "center",
                            tags$img(
                                src = "mixfishcons.png",
                                height = "200px"
                            )
                        ), br(),
                        br(),
                        tags$button(
                            id = "web_button2",
                            class = "btn action-button",
                            align = "center",
                            tags$img(
                                src = "miffishadv.png",
                                height = "200px"
                            )
                        )
                    )
                )
            )
        ),


        # style = "max-height: 90vh; overflow-y: auto; overflow-x: hidden; !important;",
        # sidebarLayout(
        #     sidebarPanel = stock_selection_left_side(),
        #     mainPanel = stock_selection_right_side()

        # )


        ########################################## New version of SAG plots ############################
        tabPanel(
            "FO"
            # style = "max-height: 90vh; overflow-y: auto; overflow-x: hidden; !important;",
            # splitLayout(
            #     cellWidths = c("40%", "60%"),
            #     header_left_panel_stock_info("stock_infos1"),
            #     header_right_panel_headline("Advice_Headline1")
            # ),
            # sidebarPanel(
            #  width = 12,
            # SAG_plots_1_2_fluid(),
            # br(),
            # SAG_plots_3_4_fluid()
            # )
        ),
        tabPanel(
            "MixFish",
            tabsetPanel(
               tabPanel("Headline",
                        selectizeGroupUI(
                                id = "my-filters",
                                params = list(
                                    scenario = list(inputId = "scenario", title = "scenario:"),
                                    stock = list(inputId = "stock", title = "stock:")
                                    # indicator = list(inputId = "indicator", title = "indicator:")
                                ),
                                inline = TRUE
                            ),
                        tags$style(type = "text/css", "#headline_bars {height: calc(99vh - 220px) !important;} overflow-y: hidden;"),
                        plotlyOutput("headline_bars", height = "100%", width = "100%")
               ),
               tabPanel("Spatial landings",
                        sidebarLayout(
                            sidebarPanel(
                                width = 3,
                                # selectizeInput(inputId = "gear_type", 
                                #             label = "Gear type:",
                                #             selected = c("OTB", "OTM"),
                                #             choices = c("OTB", "OTM"),
                                #             multiple = TRUE),
                                # selectizeInput(inputId = "species", 
                                #             label = "Species:", 
                                #             selected = c("COD", "HAD", "POK", "WHG"),
                                #             choices = c("COD", "HAD", "POK", "WHG"),
                                #             multiple = TRUE)
                            selectizeGroupUI(
                                id = "fdi_filters",
                                params = list(
                                    species = list(inputId = "species", title = "Species:"),
                                    gear_type = list(inputId = "gear_type", title = "Gear type:"),
                                    ecoregion = list(inputId = "ecoregion", title = "ecoregion:")
                                ),
                                inline = TRUE
                            )
                            ),
                            mainPanel(
                                width = 9,
                                fluidRow(
                                    column(5, plotOutput("map_species", width = "100%", height = 600)),
                                    column(5, plotOutput("map_gear_type", width = "100%", height = 600))
                                )
                            )
                            )
               ),
               tabPanel("effort",
               tags$style(type = "text/css", "#effort {height: calc(99vh - 220px) !important;} overflow-y: hidden;"),
                        plotlyOutput("effort_plot", height = "100%", width = "100%"))
             )
            # style = "overflow-y: auto; overflow-x: hidden;",
            # splitLayout(
            #     cellWidths = c("40%", "60%"),
            #     header_left_panel_stock_info("stock_infos2"),
            #     header_right_panel_headline("Advice_Headline2")
            # ),
            # quality_of_assessment_fluid()
        ),

        ######################################################################################################

        # tabPanel(
        #     "Catch scenarios",
        #     style = " max-height: 90vh; overflow-y: auto; overflow-x: hidden; !important;",
        #     splitLayout(
        #         cellWidths = c("40%", "60%"),
        #         header_left_panel_stock_info("stock_infos3"),
        #         header_right_panel_headline("Advice_Headline3")
        #     ),
        #     sidebarLayout(
        #         sidebarPanel = catch_scenarios_left_panel(),
        #         mainPanel = catch_scenarios_right_panel()
        #     )

        # ),
        # navbarMenu(
        #         "Resources",
        #         tabPanel(
        #             "Contact & feedback",
        #             htmlOutput("contact_feedback")
        #         ),
        #         tabPanel(
        #             "Data sources",
        #             htmlOutput("data_sources")
        #         ),
        #         tabPanel(
        #             "Data disclaimer & policy",
        #             htmlOutput("data_disclaimer_policy")
        #         ),
        #         tabPanel(
        #             "Citation",
        #             htmlOutput("citation")
        #         )
        #     )
    )
)




