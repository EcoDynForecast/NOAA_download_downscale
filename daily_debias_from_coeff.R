daily_debias_from_coeff <- function(daily_forecast, coeff.df){
  # does linear debiasing from previously calculated coefficients (coeff.df)

# lin.mod
# @param: coeff, the coeffients (from coeff.df) that correspond to the variable of interest
# @param: col.for, the column of daily_forecast that corresponds to the variable of interest
  lin.mod <- function(col.for, coeff){
    slope = coeff[1]
    intercept = coeff[2]
    modeled = col.for*slope + intercept
    return(modeled)
  }
  
  debiased <- daily_forecast %>% 
    dplyr::mutate(temp.mod =  lin.mod(daily_forecast$temp, coeff.df$temp),
                  RH.mod =  lin.mod(daily_forecast$RH, coeff.df$RH),
                  ws.mod =  lin.mod(daily_forecast$ws, coeff.df$ws),
                  sw.mod =  lin.mod(daily_forecast$sw, coeff.df$sw),
                  lw.mod =  lin.mod(daily_forecast$lw, coeff.df$lw),
                  dscale.member = 0) %>% 
    select(date, NOAA.member, dscale.member, temp.mod, RH.mod, ws.mod, sw.mod, lw.mod)
  return(debiased)
}

