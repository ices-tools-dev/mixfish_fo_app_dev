library(mixfishtools)
library(dplyr)
library(plotly)







### UI part of the module
select_group_ui(
    id = "my-filters",
    params = list(
        fleet = list(inputId = "fleet", title = "Fleet:"),
        stock = list(inputId = "stock", title = "Stock:"),
        Advice_name = list(inputId = "Advice_name", title = "Advice name:"),
        Limitation = list(inputId = "Limitation", title = "Limitation:")
    ),
    inline = TRUE
)

####Server part of the module
#### data formatting
data_reactive <- reactive({
data(refTable) # reference table with stock advice names, colors, order, etc.
data(stfFltSum) # summary of fleet-related variables (e.g. effort)
data(stfFltStkSum) # summary of fleet/stock-related catch variables
advYr <- 2022 # advice year
df <- subset(stfFltStkSum, scenario == "min" & year == advYr)
eff <- subset(
stfFltSum, scenario == "min" & year == advYr)[,c("fleet", "effort")]
sqEff <- subset(
stfFltSum, scenario == "sq_E" & year == advYr)[,c("fleet", "effort")]
names(sqEff)[2] <- "sqEffort"
eff <- merge(x = eff, y = sqEff, all.x = TRUE)
df <- merge(x = df, y = eff, all.x = TRUE)
df$quotaEffort <- df$effort / df$quotaUpt
#'
restr.stks <- c("COD-NS", "HAD", "PLE-EC", "PLE-NS", "POK", "SOL-EC",
"SOL-NS", "TUR", "WHG-NS", "WIT", "NEP6", "NEP7", "NEP8", "NEP9")
fls <- unique(df$fleet)
df2 <- vector("list", length(fls))
names(df2) <- fls
for(i in seq(fls)){
tmp <- subset(df, fleet == fls[i])
tmp$Limitation <- NA # initial NA setting for all stocks
 mostLimStk <- subset(tmp, stock %in% restr.stks)
mostLimStk <- mostLimStk$stock[which.max(mostLimStk$quotaUpt)]
tmp$Limitation[which(tmp$stock == mostLimStk)] <- "most"
leastLimStk <- subset(stfFltStkSum, scenario == "max" & year == advYr &
fleet == fls[i] & stock %in% restr.stks)
leastLimStk <- leastLimStk$stock[which.min(leastLimStk$quotaUpt)]
tmp$Limitation[which(tmp$stock == leastLimStk)] <- "least"
df2[[i]] <- tmp
 }
df2 <- do.call("rbind", df2)
#' # add Advice_name corresponding to refTable
df2 <- merge(x = df2, y = refTable[,c("stock", "Advice_name")], all.x = TRUE)
})


res_mod <- select_group_server(
    id = "my-filters",
    data = data_reactive(),
    vars = reactive(c("fleet", "stock", "Advice_name", "Limitation"))
  )

ggplotly(plot_effortFltStk(data = res_mod(), refTable = refTable))
