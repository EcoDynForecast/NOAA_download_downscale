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
                  sw.mod =  lin.mod(daily_forecast$sw, coeff.df$sw)) %>% 
    select(date, NOAA.member, temp.mod, RH.mod, ws.mod, sw.mod)
  
  # 
  debiased.with.noise <- debiased %>%
    group_by(date, NOAA.member, temp.mod, RH.mod,  ws.mod, sw.mod) %>%
    expand(dscale.member = 1:nmembers) %>%
    ungroup() %>%
    group_by(date, dscale.member, NOAA.member, temp.mod, RH.mod,  ws.mod, sw.mod) %>%
    dplyr::mutate(temp.mod.noise = temp.mod + rnorm(mean = 0, sd = coeff.df$temp[3], n = 1),
                  RH.mod.noise = RH.mod + rnorm(mean = 0, sd = coeff.df$RH[3], n = 1),
                  ws.mod.noise = ws.mod + rnorm(mean = 0, sd = coeff.df$ws[3], n = 1),
                  sw.mod.noise = sw.mod + rnorm(mean = 0, sd = coeff.df$sw[3], n = 1)) %>%
    ungroup() %>%
    select(date, dscale.member, NOAA.member, temp.mod, temp.mod.noise, RH.mod, RH.mod.noise, ws.mod, ws.mod.noise, sw.mod, sw.mod.noise)
  
  return(list(debiased, debiased.with.noise))
}

