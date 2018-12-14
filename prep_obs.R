prep_obs <- function(obs.data){
  # --------------------------------------
  # purpose: convert observations dataframe to format/units for comparison with forecasts
  # Creator: Laura Puckett, December 14 2018
  # --------------------------------------
  
  obs.units.match <- obs.data %>%
    dplyr::mutate(date = as.Date(TIMESTAMP, format = '%m/%d/%y')) %>%
    separate(TIMESTAMP, c("date.extra","time")," ", convert = TRUE) %>%
    dplyr::mutate(yday = lubridate::yday(date)) %>%
    separate(time, c("hour","minute"),":", convert = TRUE) %>%
    dplyr::mutate(timestamp = as_datetime(paste(date, " ", hour, ":", minute,":00", sep = ""),
                                          tz = "US/Eastern")) %>%
    dplyr::mutate(date = as_date(date),
           precip_rate = Rain_mm_Tot/60) %>%
           mutate(AirTK_Avg = AirTC_Avg + 273.15) %>% # convert from C to Kelvin
    select(timestamp, AirTK_Avg, RH, WS_ms_Avg, SR01Up_Avg, IR01UpCo_Avg, precip_rate)
  
  return(obs.units.match)
}
