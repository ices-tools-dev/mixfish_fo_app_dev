#' fleet_stock_effort 
#'
#' @description a function to calculate and plot the effort per fleet by stock
#' 
#' @param df 
#' @param eff 
#' @param advice_year 
#' @param upper_lim 
#'
#' @importFrom plotly ggplotly
#' 
compute_fleet_stock_effort <- function(df, eff, advice_year = NULL, upper_lim = 100) {
  
  if(is.null(advice_year)){
    advice_year <- lubridate::year(Sys.Date())-1
  }
  
  df <- subset(stfFltStkSum, scenario == "min" & year == advice_year)
  eff <- subset(
    stfFltSum, scenario == "min" & year == advice_year)[,c("fleet", "effort")]
  sqEff <- subset(
    stfFltSum, scenario == "sq_E" & year == advice_year)[,c("fleet", "effort")]
  names(sqEff)[2] <- "sqEffort"
  eff <- merge(x = eff, y = sqEff, all.x = TRUE)
  df <- merge(x = df, y = eff, all.x = TRUE)
  df$quotaEffort <- df$effort / df$quotaUpt
  df$relEffort <- df$quotaEffort / df$sqEffort
  df$scenario <- df$stock
  restr.stks <- c("COD-NS", "HAD", "PLE-EC", "PLE-NS", "POK", "SOL-EC",
                  "SOL-NS", "TUR", "WHG-NS", "WIT", "NEP6", "NEP7", "NEP8", "NEP9")
  df <- subset(df, stock %in% restr.stks)
  
  # convert to percentage change
  df$var <- 100*(df$relEffort-1)
  
  # optional upper limit (e.g. 100)
  df$var <- ifelse(df$var > upper_lim, upper_lim, df$var)
  return(df)
}
  