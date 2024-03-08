#' fleet_stock_effort 
#'
#' @description a function to calculate the relative effort per fleet by stock
#' 
#' @param df 
#' @param eff 
#' @param advice_year 
#' @param upper_lim 
#' @importFrom lubridate year
#'
compute_fleet_stock_effort <- function(input_df, input_effort, advice_year = NULL, upper_lim = 100, scenario) {
  
  if(is.null(advice_year)){
    advice_year <- year(Sys.Date())-1
  }
  
  df <- compute_quota_effort(input_df, input_effort, advice_year = advice_year, scenario_var = scenario)
  
  
  restr.stks <- c("COD-NS", "HAD", "PLE-EC", "PLE-NS", "POK", "SOL-EC", "SOL-NS", "TUR", "WHG-NS", "WIT", "NEP6", "NEP7", "NEP8", "NEP9")
  
    
    df$relEffort <- df$quotaEffort / df$sqEffort
    df$scenario <- df$stock
    df2 <- subset(df, stock %in% restr.stks)
    
    # convert to percentage change
    df2$var <- 100*(df2$relEffort-1)
    
    # optional upper limit (e.g. 100)
    df2$var <- ifelse(df2$var > upper_lim, upper_lim, df2$var)
  
  return(df2)
}


#' most and least limiting stocks
#'
#' @description a function to calculate the most and least limiting stocks
#' 
#' @param df 
#' @param eff 
#' @param advice_year 
#' @param upper_lim 
#' @importFrom lubridate year
#'
compute_limiting_stocks <- function(input_df, input_effort, advice_year = NULL) {
  
  if(is.null(advice_year)){
    advice_year <- year(Sys.Date())-1
  }

  df <- compute_quota_effort(input_df, input_effort, advice_year = advice_year)
  
  restr.stks <- c("COD-NS", "HAD", "PLE-EC", "PLE-NS", "POK", "SOL-EC", "SOL-NS", "TUR", "WHG-NS", "WIT", "NEP6", "NEP7", "NEP8", "NEP9")
  
 
    
    fls <- unique(df$fleet)
    df2 <- vector("list", length(fls))
    names(df2) <- fls
    for (i in seq(fls)) {
      tmp <- subset(df, fleet == fls[i])
      tmp$Limitation <- NA # initial NA setting for all stocks
      mostLimStk <- subset(tmp, stock %in% restr.stks)
      mostLimStk <- mostLimStk$stock[which.max(mostLimStk$quotaUpt)]
      tmp$Limitation[which(tmp$stock == mostLimStk)] <- "most"
      
      leastLimStk <- subset(input_df, scenario == "max" & year == advice_year &
                              fleet == fls[i] & stock %in% restr.stks)
      leastLimStk <- leastLimStk$stock[which.min(leastLimStk$quotaUpt)]
      tmp$Limitation[which(tmp$stock == leastLimStk)] <- "least"
      df2[[i]] <- tmp
    }
    df2 <- do.call("rbind", df2)
    
    # add Advice_name corresponding to refTable
    df2 <- merge(x = df2, y = refTable[, c("stock", "Advice_name")], all.x = TRUE)
    
  return(df2)
}
  


#' Compute effort required to meet quota 
#'
#' @param df 
#' @param input_effort 
#' @param scenario 
#' @param year 
#'
#' @return
#' @export
#'
#' @examples
compute_quota_effort <- function(input_df, input_effort, scenario_var = "min", advice_year){
  
    df <- subset(input_df, scenario == scenario_var & year == advice_year)
    eff <- subset(input_effort, scenario == scenario_var & year == advice_year)[,c("fleet", "effort")]
    validate(
      need(nrow(df)!=0, "Invalid combination of filters"),
      need(nrow(eff)!=0, "Invalid combination of filters")
    )
    sqEff <- subset(input_effort, scenario == "sq_E" & year == advice_year)[,c("fleet", "effort")]
    names(sqEff)[2] <- "sqEffort"
    
    eff <- merge(x = eff, y = sqEff, all.x = TRUE)
    df <- merge(x = df, y = eff, all.x = TRUE)
    df$quotaEffort <- df$effort / df$quotaUpt
    
    return(df)
}

