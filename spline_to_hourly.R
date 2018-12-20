spline_to_hourly <- function(redistributed){
  # --------------------------------------
  # purpose: interpolates debiased forecasts from 6-hourly to hourly
  # Creator: Laura Puckett, December 16 2018
  # --------------------------------------
  # @param: redistributed, a dataframe of debiased 6-hourly forecasts
  
  interpolate <- function(jday, var){
    result <- splinefun(jday, var, method = "monoH.FC")
    return(result(seq(min(as.numeric(jday)), max(as.numeric(jday)), 1/24)))
  }
  
  time0 = as_datetime(min(redistributed$timestamp), tz = "US/Eastern")
  redistributed <- redistributed %>%
    mutate(days_since_t0 = difftime(.$timestamp, time0, units = "days"))
  
  by.ens <- redistributed %>% 
    group_by(NOAA.member)
  
  interp.df.days <- by.ens %>% do(days = seq(min(redistributed$days_since_t0), as.numeric(max(redistributed$days_since_t0)), 1/24))
  interp.df <- interp.df.days
  
  for(Var in 1:length(VarNamesStates)){
    assign(paste0("interp.df.",VarNamesStates[Var]), do(by.ens, var = interpolate(.$days_since_t0,unlist(.[,VarNamesStates[Var]]))) %>% plyr::rename(c("var" = VarNamesStates[Var])))
    interp.df <- inner_join(interp.df, get(paste0("interp.df.",VarNamesStates[Var])), by = c("NOAA.member"))
  }

  # converting from time difference back to timestamp
  interp.df  = interp.df %>%
    unnest %>%
    dplyr::mutate(timestamp = as_datetime(time0 + days, tz = "US/Eastern"))
  
  return(interp.df)
}

