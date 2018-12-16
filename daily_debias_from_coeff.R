daily_debias_from_coeff <- function(daily_forecast, coeff.df){
  # --------------------------------------
  # purpose: does linear debiasing from previously calculated coefficients
  # Creator: Laura Puckett, December 14 2018
  # --------------------------------------
  # @param: daily_forecast, dataframe of past forecasts at daily resolution
  # @param: coeff.df, the save coefficients for linear debaiasing for each meterological variable at daily resolution

  lin.mod <- function(col.for, coeff){
    intercept = coeff[1]
    slope = coeff[2]
    modeled = col.for*slope + intercept
    return(modeled)
  }
  
  debiased <- daily_forecast %>% 
    dplyr::mutate(temp.mod =  lin.mod(daily_forecast$temp, coeff.df$temp),
                    RH.mod =  lin.mod(daily_forecast$RH, coeff.df$RH),
                    ws.mod =  lin.mod(daily_forecast$ws, coeff.df$ws),
                    sw.mod =  lin.mod(daily_forecast$sw, coeff.df$sw),
                    lw.mod =  lin.mod(daily_forecast$lw, coeff.df$lw),
                    dscale.member = 0) %>% # dscale.member just a placeholder at this step; downscale members are not created yet
    select(date, NOAA.member, dscale.member, temp.mod, RH.mod, ws.mod, sw.mod, lw.mod)
  return(debiased)
}

