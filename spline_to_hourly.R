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
    group_by(NOAA.member, dscale.member)
    
  interp.df.days <- by.ens %>% do(days = seq(min(redistributed$days_since_t0), as.numeric(max(redistributed$days_since_t0)), 1/24))
  interp.df.temp <- do(by.ens, interp.temp = interpolate(.$days_since_t0,.$ds.temp))
  interp.df.ws <- do(by.ens, interp.ws = interpolate(.$days_since_t0,.$ds.ws))
  interp.df.RH <- do(by.ens, interp.RH = interpolate(.$days_since_t0,.$ds.RH))
  interp.df <- inner_join(interp.df.days, interp.df.temp, by = c("NOAA.member","dscale.member")) %>%
    inner_join(interp.df.ws, by = c("NOAA.member","dscale.member")) %>%
    inner_join(interp.df.RH,  by = c("NOAA.member","dscale.member")) %>%
    unnest()
  
  # converting from time difference back to timestamp
  interp.df  = interp.df %>%
    dplyr::mutate(timestamp = as_datetime(time0 + days, tz = "US/Eastern"))

  return(interp.df)
}

