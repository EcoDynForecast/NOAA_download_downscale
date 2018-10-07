agg_and_join <- function(obs.units.match, forecast.units.match){
  timestamp.0 = as_datetime("2018-04-05 08:00:00 EDT") # beginning of first 6-hour period
  
  obs.6.hourly <- obs.units.match %>% 
    mutate(group = as.integer(difftime(timestamp,timestamp.0, units = c("mins"))/(60*6))) %>%
    group_by(group) %>% # create groups for each 6-hour time period
    dplyr::mutate(precip.rate = mean(precip_rate, na.rm = TRUE),
                  avg.lw = mean(IR01UpCo_Avg, na.rm = TRUE),
                  avg.sw = mean(SR01Up_Avg, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(hour(timestamp)) %>%
    mutate(ws = mean(WS_ms_Avg, na.rm = TRUE)) %>% # hourly average wind_speed 
    ungroup() %>%
    filter(hour(timestamp) %in% c(1,7,13,19) & minute(timestamp) == 0) %>% 
    dplyr::rename(temp = AirTC_Avg)
  
  joined.6.hourly = dplyr::inner_join(obs.6.hourly %>% select(timestamp, doy, temp, avg.lw, avg.sw, precip.rate, RH, ws), forecast.units.match %>% select(timestamp, doy, NOAA.member, temp, avg.lw, avg.sw, precip.rate, RH, ws), by= c("timestamp", "doy"), suffix = c(".obs",".for")) 
  saveRDS(joined.6.hourly, file = paste(path.working, "my_files/","joined.6.hourly", sep = ""))
  return(joined.6.hourly)
}