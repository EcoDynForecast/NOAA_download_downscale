match_units <- function(obs.data, NOAA.data){
  # --------------------------------------
  # 1. Align datasets (get common dates and units)
  # --------------------------------------
  # 
  
  # 1.0 make date formats match. "date" is the day of the year as "YYYY-MM-DD" and "timestamp" is the date + time as "YYYY-MM-DD HH:MM:SS"
  
  forecast.data <- NOAA.data %>%
      #rename(c("forecast.date" = "date")) %>%
      dplyr::mutate(timestamp = as_datetime(forecast.date.hour, tz = "US/Eastern"),
                    timestamp = timestamp - 3600) %>% # timezone part is still wrong...temporary hack
    # attributes(forecast.data$timestamp)$tzone <- "US/Eastern"
    # forecast.data <- forecast.data %>%
      # dplyr::mutate(yday = lubridate::yday(as_date(timestamp)),
       #             hour = hour(timestamp)) %>%
                    # doy = formattable(round(yday(timestamp) + hour(timestamp)/24,4),4)) %>%
      rename(c("ensembles" = "NOAA.member"))
    
    
  obs.units.match.1 <- obs.data %>%
    dplyr::mutate(date = as.Date(TIMESTAMP, format = '%m/%d/%y')) %>%
    separate(TIMESTAMP, c("date.extra","time")," ", convert = TRUE) %>%
    dplyr::mutate(yday = lubridate::yday(date)) %>%
    separate(time, c("hour","minute"),":", convert = TRUE) %>%
    dplyr::mutate(timestamp = as_datetime(paste(date, " ", hour, ":", minute,":00", sep = ""), tz = "US/Eastern"))
  # attributes(obs.units.match.1$timestamp)$tzone <- "US/Eastern" # not sure if this step is required
  
  obs.units.match <- obs.units.match.1 %>% dplyr::mutate(date = as_date(date),
           # doy = yday(timestamp) + hour(timestamp)/24 + minute(timestamp)/(24*60),
           precip_rate = Rain_mm_Tot/60) %>%
    select(timestamp, AirTC_Avg, RH, WS_ms_Avg, SR01Up_Avg, IR01DnCo_Avg, precip_rate) 
  
  # saveRDS(obs.units.match, file = paste(path.working,"obs.units.match.RData",sep= ""))
  
  forecast.units.match <- forecast.data %>%
    dplyr::mutate(air_temperature = air_temperature - 273.15, # convert from K to C
           wind_speed = sqrt(eastward_wind^2 + northward_wind^2),
           surface_downwelling_longwave_flux_in_air = ifelse(surface_downwelling_longwave_flux_in_air==999900000000000000000, NA,surface_downwelling_longwave_flux_in_air),
           surface_downwelling_shortwave_flux_in_air = ifelse(surface_downwelling_shortwave_flux_in_air==999900000000000000000, NA,surface_downwelling_shortwave_flux_in_air),
           precipitation_flux = ifelse(precipitation_flux==999900000000000000000, NA, precipitation_flux)) %>%
    rename(c("air_temperature" = "temp",
             "surface_downwelling_longwave_flux_in_air" = "avg.lw",
             "surface_downwelling_shortwave_flux_in_air" = "avg.sw",
             "precipitation_flux" = "precip.rate",
             "relative_humidity" = "RH",
             "wind_speed" = "ws")) %>%
    select(NOAA.member, timestamp, temp, avg.lw, avg.sw, precip.rate, RH, ws)
  #bsaveRDS(forecast.units.match, file = paste(path.working,"/forecast.units.match.RData",sep= ""))
  return(list(obs.units.match, forecast.units.match))
}
