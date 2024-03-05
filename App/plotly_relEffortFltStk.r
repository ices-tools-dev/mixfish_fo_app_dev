library(mixfishtools)
library(dplyr)
library(plotly)


data(stfFltSum) # summary of fleet-related variables (e.g. effort)
data(stfFltStkSum) # summary of fleet/stock-related catch variables


### Data formatting

advYr <- 2022 # advice year
df <- subset(stfFltStkSum, scenario == "min" & year == advYr)
eff <- subset(
    stfFltSum, scenario == "min" & year == advYr
)[, c("fleet", "effort")]
sqEff <- subset(
    stfFltSum, scenario == "sq_E" & year == advYr
)[, c("fleet", "effort")]
names(sqEff)[2] <- "sqEffort"
eff <- merge(x = eff, y = sqEff, all.x = TRUE)
df <- merge(x = df, y = eff, all.x = TRUE)
df$quotaEffort <- df$effort / df$quotaUpt
df$relEffort <- df$quotaEffort / df$sqEffort
df$scenario <- df$stock
restr.stks <- c(
    "COD-NS", "HAD", "PLE-EC", "PLE-NS", "POK", "SOL-EC",
    "SOL-NS", "TUR", "WHG-NS", "WIT", "NEP6", "NEP7", "NEP8", "NEP9"
)
df <- subset(df, stock %in% restr.stks)
#' # convert to percentage change
df$var <- 100 * (df$relEffort - 1)
#' # optional upper limit (e.g. 100)
df$var <- ifelse(df$var > 100, 100, df$var)


### Plotting function

plotly_relEffortFltStk <- function(
    data,
    limits = c(-100, 100),
    xlab = "Stock",
    ylab = "Fleet",
    fillLegendTitle = "Variation\n in effort") {
    p <- ggplot(data) +
        aes(x = scenario, y = fleet) +
        geom_tile(aes(fill = var)) +
        scale_fill_gradient2(
            name = "Variation\n in effort",
            limits = c(-100, 100),
            low = "red4",
            mid = "grey90",
            high = "blue4",
            midpoint = 0
        ) +
        theme(
            panel.background = element_blank(),
            strip.background = element_rect(colour = NA, fill = NA),
            axis.text.x = element_text(hjust = 0, vjust = 0.3),
            text = element_text(size = 13)
        ) +
        xlab(xlab) +
        ylab(ylab)

    return(ggplotly(p))
}
#p <- plotly_relEffortFltStk(data = df)
