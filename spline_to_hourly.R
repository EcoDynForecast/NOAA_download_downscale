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
  by.ens <- redistributed %>% 
    group_by(NOAA.member, dscale.member) %>%
    mutate(jday = julian(timestamp, origin = "1970-01-01 00:00:00"))
  # convert to julian day (jday) temporarily to interpolate between times
  
  interp.df.jday <- by.ens %>% do(jday = seq(as.numeric(min(julian(redistributed$timestamp, origin = "1970-01-01 00:00:00"))), as.numeric(max(julian(redistributed$timestamp, origin = "1970-01-01 00:00:00"))), 1/24))
  interp.df.temp <- do(by.ens, interp.temp = interpolate(.$jday,.$ds.temp))
  interp.df.ws <- do(by.ens, interp.ws = interpolate(.$jday,.$ds.ws))
  interp.df.RH <- do(by.ens, interp.RH = interpolate(.$jday,.$ds.RH))
  interp.df <- inner_join(interp.df.jday, interp.df.temp, by = c("NOAA.member","dscale.member")) %>%
    inner_join(interp.df.ws, by = c("NOAA.member","dscale.member")) %>%
    inner_join(interp.df.RH,  by = c("NOAA.member","dscale.member")) %>%
    unnest()
  
  # converting from julian days back to timestamp
  interp.df  = interp.df %>%
    dplyr::mutate(date = as.Date(jday, origin = as.Date("1970-01-01")),
                hour = round((jday - as.integer(jday))*24,0),
                timestamp = as_datetime(paste(date, " ", hour, ":","00:00", sep = ""), tz = "US/Eastern"))

  return(interp.df)
}

