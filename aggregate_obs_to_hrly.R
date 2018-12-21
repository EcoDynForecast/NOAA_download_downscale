aggregate_obs_to_hrly <- function(observations){
  hrly.flux.obs <- observations %>%
    dplyr::mutate(date = date(timestamp)) %>%
    dplyr::mutate(hour = hour(timestamp)) %>%
    dplyr::group_by(date, hour) %>%
    dplyr::summarize(ShortWave = mean(ShortWave),
                     LongWave = mean(LongWave)) %>%
    ungroup() %>%
    dplyr::mutate(timestamp = as_datetime(paste(date, " ", hour, ":","00:00", sep = ""), tz = output_tz) + 60*60) %>% # add one hour so that timestamp represents average over past hour
    select(timestamp, ShortWave, LongWave)
  
  
  hrly.state.obs <- observations %>% group_by(timestamp) %>%
    dplyr::summarize(AirTemp = mean(AirTemp),
                     RelHum = mean(RelHum),
                     WindSpeed = mean(WindSpeed)) %>%
    ungroup()
  hrly.obs = inner_join(hrly.flux.obs, hrly.state.obs, by = "timestamp")
  return(hrly.obs)
}