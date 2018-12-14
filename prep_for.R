prep_for <- function(NOAA.data){
  # --------------------------------------
  # purpose: convert forecasts dataframe to units/names for comparison with observations
  # Creator: Laura Puckett, December 14 2018
  # --------------------------------------
  
  forecast.data <- NOAA.data %>%
    dplyr::mutate(timestamp = as_datetime(forecast.date.hour, tz = "US/Eastern"),
                  timestamp = timestamp - 60*60) %>% # timezone part is still off...temporary hack for non-daylight savings part of year
    rename(c("ensembles" = "NOAA.member"))
  
  forecast.units.match <- forecast.data %>%
    dplyr::mutate(ws = sqrt(ugrd10m^2 + vgrd10m^2), # get total wind speed from East + West wind speed
                  dlwrfsfc = ifelse(dlwrfsfc==999900000000000000000, NA,dlwrfsfc),
                  dswrfsfc = ifelse(dswrfsfc==999900000000000000000, NA,dswrfsfc),
                  pratesfc = ifelse(pratesfc==999900000000000000000, NA, pratesfc)) %>%
    rename(c("tmp2m" = "temp",
             "dlwrfsfc" = "avg.lw",
             "dswrfsfc" = "avg.sw",
             "pratesfc" = "precip.rate",
             "rh2m" = "RH")) %>%
    select(NOAA.member, timestamp, temp, avg.lw, avg.sw, precip.rate, RH, ws)
  return(forecast.units.match)
}
