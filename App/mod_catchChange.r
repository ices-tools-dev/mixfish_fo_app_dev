library(mixfishtools)
library(dplyr)
library(plotly)







### UI part of the module
select_group_ui(
    id = "my-filters",
    params = list(
        fleet = list(inputId = "fleet", title = "Fleet:"),
        stock = list(inputId = "stock", title = "Stock:"),
        scenario = list(inputId = "scenario", title = "scenario:"),
        choke = list(inputId = "choke", title = "choke:")
    ),
    inline = TRUE
)


#### Server part of the module
#### data formatting
data_reactive <- reactive({
    data(refTable) # reference table with stock advice names, colors, order, etc.
    data(stfFltStkSum) # summary of fleet/stock-related catch variables
    advYr <- 2022 # advice year
    #'
})


res_mod <- select_group_server(
    id = "my-filters",
    data = data_reactive(),
    vars = reactive(c("fleet", "stock", "scenario", "choke"))
)

### plotting
p1 <- plot_catch_change(
    data = stfFltStkSum,
    basis = "Quota",
    dataYrs = 2020:2022,
    advYr = advYr,
    sc = "min",
    fleets_excl = "OTH_OTH",
    refTable = refTable,
    xlab = "Stock",
    ylab = "landings change (tonnes)",
    fillLegendTitle = "Stock",
    colLegendTitle = "Limiting stock"
)


ggplotly(p1)
