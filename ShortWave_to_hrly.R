ShortWave_to_hrly <- function(debiased, lat, lon){
  ## downscale shortwave to hourly
  
  ShortWave.hours <- debiased %>%
    dplyr::group_by(NOAA.member, date) %>%
    tidyr::expand(hour = 0:23)
  
  ShortWave.ds <- debiased %>% 
    select(ShortWave, NOAA.member, date) %>%
    dplyr::group_by(NOAA.member, date) %>%
    full_join(ShortWave.hours, by = c("NOAA.member","date")) %>%
    ungroup() %>%
    dplyr::mutate(timestamp = as_datetime(paste(date, " ", hour, ":","00:00", sep = ""), tz = "US/Eastern")) %>%
    dplyr::mutate(doy = yday(date) + hour/24) %>%
    # convert to UTC for use in solar_geom function (accounting for daylight savings in conversion)
    dplyr::mutate(doy.UTC = ifelse(doy >= 69 + 2/24 & doy < 307 + 2/24, 
                                   doy + 3/24, # adjust this later (should be 5 hrs difference?)
                                   doy + 2/24)) %>% # could account for leap years later
    dplyr::mutate(rpot = solar_geom(doy.UTC, lon, lat)) %>% # hourly sw flux calculated using solar geometry
    dplyr::group_by(date) %>%
    dplyr::mutate(avg.rpot = mean(rpot)) %>% # daily sw mean from solar geometry
    ungroup() %>%
    dplyr::mutate(ShortWave = ifelse(avg.rpot > 0, ShortWave * (rpot/avg.rpot),0)) %>%
    select(timestamp, NOAA.member, ShortWave)
}