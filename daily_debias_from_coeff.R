daily_debias_from_coeff <- function(daily.forecast, coeff.df){
  # --------------------------------------
  # purpose: does linear debiasing from previously calculated coefficients
  # Creator: Laura Puckett, December 14 2018
  # --------------------------------------
  # @param: daily.forecast, dataframe of past forecasts at daily resolution
  # @param: coeff.df, the save coefficients for linear debaiasing for each meterological variable at daily resolution

  lin_mod <- function(col.for, coeff){
    intercept = coeff[1]
    slope = coeff[2]
    modeled = col.for*slope + intercept
    return(modeled)
  }
  debiased = daily.forecast %>% select(date, NOAA.member)
  for(Var in 1:length(VarNames)){
    assign(VarNames[Var], value = as_data_frame(lin_mod(daily.forecast[,VarNames[Var]],
                                          coeff.df[,VarNames[Var]])))
    debiased <- cbind(debiased, get(VarNames[Var]))
  }
  return(debiased)
}

