prep_for <- function(NOAA.data){
  # --------------------------------------
  # purpose: convert forecasts dataframe to units/names for comparison with observations
  # Creator: Laura Puckett, December 14 2018
  # --------------------------------------
  NOAA.na.value = 999900000000000000000
  forecast.data <- NOAA.data %>%
    dplyr::mutate(timestamp = as_datetime(forecast.date, tz = "US/Eastern")) %>%
    plyr::rename(c("ensembles" = "NOAA.member")) %>%
    dplyr::mutate(AirTemp = tmp2m,
                  WindSpeed = sqrt(vgrd10m^2 + ugrd10m^2),
                  LongWave = ifelse(dlwrfsfc==NOAA.na.value, NA, dlwrfsfc),
                  ShortWave = ifelse(dswrfsfc==NOAA.na.value, NA, dswrfsfc),
                  RelHum = rh2m,
                  Rain = pratesfc*60*60*24*0.001) %>%
    select(NOAA.member, timestamp, AirTemp, LongWave, ShortWave, RelHum, WindSpeed, Rain)
  return(forecast.data)
}
