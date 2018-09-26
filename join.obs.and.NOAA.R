join.obs.and.NOAA <- function(obs.data, NOAA.data){
  # --------------------------------------
  # 1. Align datasets (get common dates and units)
  # --------------------------------------
  # 
  
  # 1.0 make date formats match. "date" is the day of the year as "YYYY-MM-DD" and "timestamp" is the date + time as "YYYY-MM-DD HH:MM:SS"
  
  forecast.data <- NOAA.data %>%
    rename(c("forecast.date" = "date")) %>%
    dplyr::mutate(date = as_date(date),
                  timestamp = as_datetime(forecast.date.hour, tz = "US/Eastern"),
                  doy = lubridate::yday(date),
                  hour = hour(forecast.date.hour))
  
  obs <- obs.data %>%
    separate(TIMESTAMP, c("date","time")," ", convert = TRUE) %>%
    separate(date, c("month", "day","year"), "/") %>%
    separate(time, c("hour","minute"),":", convert = TRUE) %>%
    dplyr::mutate(month = ifelse(month < 10, paste(0, month, sep = ""), month),
                  day = ifelse(day < 10, paste(0, day, sep = ""), day)) %>%
    dplyr::mutate(date = paste(year, "-", month, "-", day, sep = ""),
                  doy = lubridate::yday(date)) %>%
    dplyr::mutate(timestamp = as_datetime(paste(date, " ", hour, ":", minute,":00", sep = ""), tz = "US/Eastern"),
                  date = as_date(date))
  
  # 1.1 make units match
  forecast.units.match <- forecast.data %>%
    mutate(air_temperature = air_temperature - 273.15, # convert from K to C
           wind_speed = sqrt(eastward_wind^2 + northward_wind^2), 
           surface_downwelling_longwave_flux_in_air = ifelse(surface_downwelling_longwave_flux_in_air==999900000000000000000, NA,surface_downwelling_longwave_flux_in_air),
           surface_downwelling_shortwave_flux_in_air = ifelse(surface_downwelling_shortwave_flux_in_air==999900000000000000000, NA,surface_downwelling_shortwave_flux_in_air),
           precipitation_flux = ifelse(precipitation_flux==999900000000000000000, NA, precipitation_flux)) %>%
    rename(c("air_temperature" = "temp",
             "surface_downwelling_longwave_flux_in_air" = "avg.lw",
             "surface_downwelling_shortwave_flux_in_air" = "avg.sw",
             "precipitation_flux" = "precip.rate",
             "relative_humidity" = "RH",
             "wind_speed" = "avg.ws"))
  
  obs.units.match <- obs %>%
    dplyr::mutate(precip_rate = Rain_mm_Tot/60) # convert from mm/min to kg/m2/s
  # --------------------------------------
  # 2. Aggregate to 6-hourly
  # --------------------------------------
  # 
  
  timestamp.0 = as_datetime("2018-04-05 08:00:00 EDT") # beginning of first 6-hour period
  
  obs.6.hourly <- obs.units.match %>% 
    mutate(group = as.integer(difftime(timestamp,timestamp.0, units = c("mins"))/(60*6))) %>%
    group_by(group) %>% # create groups for each 6-hour time period
    dplyr::mutate(precip.rate = mean(precip_rate, na.rm = TRUE),
                  avg.ws = mean(WS_ms_Avg, na.rm = TRUE),
                  avg.lw = mean(IR01UpCo_Avg, na.rm = TRUE),
                  avg.sw = mean(SR01Up_Avg, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(hour %in% c(2,8,14,20) & minute(timestamp) == 0) %>% 
    dplyr::rename(temp = AirTC_Avg)
  
  joined.6.hourly = inner_join(obs.6.hourly %>% select(timestamp, temp, avg.lw, avg.sw, precip.rate, RH, avg.ws), forecast.units.match %>% select(timestamp, ensembles, temp, avg.lw, avg.sw, precip.rate, RH, avg.ws), by = "timestamp", suffix = c(".obs",".for")) %>%
    rename(c("ensembles" = "NOAA.member"))
  saveRDS(joined.6.hourly, file = paste(path.working, "my_files/","joined.6.hourly", sep = ""))
  return(joined.6.hourly)
}
