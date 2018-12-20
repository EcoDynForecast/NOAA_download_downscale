# --------------------------------------
# purpose: downscale future GEFS forecasts to FCR site and hourly resolution
# Creator: Laura Puckett, December 16 2018
# contact: plaura1@vt.edu
# --------------------------------------

# -----------------------------------
# 2. change forecast units
# -----------------------------------
forecasts <- prep_for(d)
forecasts[which(forecasts$timestamp == min(forecasts$timestamp)),]$ShortWave = forecasts[which(forecasts$timestamp == min(forecasts$timestamp) + 24*60*60),]$ShortWave
# hack to give sw values for 1st measurement (that are in fact the values for the second day). This is to avoid having NAs for the first few hours of forecast
forecasts[which(forecasts$timestamp == min(forecasts$timestamp)),]$LongWave = forecasts[which(forecasts$timestamp == min(forecasts$timestamp) + 6*60*60),]$LongWave
# hack to give lw values for 1st measurement (that are in fact the values of the next measurement, 6 hours later). This is to avoid having NAs for the first few hours of forecast

run_future_downscaling <- function(forecasts){

  
  # -----------------------------------
  # 3. Aggregate forecast to daily resolution
  # -----------------------------------
  
  daily.forecast = aggregate_to_daily(forecasts)
  # need to add in statement here for if(CALCULATE_DEBIAS_COEFFICIENTS){}
  
  # -----------------------------------
  # 4. do linear debiasing at daily resolution
  # -----------------------------------
  
  debiased <- daily_debias_from_coeff(daily.forecast, debiased.coefficients)
  
  # -----------------------------------
  # 5.a. temporal downscaling step (a): redistribute to 6-hourly resolution
  # -----------------------------------
  
  redistributed = daily_to_6hr(forecasts)
  
  # -----------------------------------
  # 5.b. temporal downscaling step (b): temporally downscale from 6-hourly to hourly
  # -----------------------------------
  
  ## downscale states to hourly resolution (air temperature, relative humidity, average wind speed) 
  states.ds.hrly = spline_to_hourly(redistributed)
  # if filtering out incomplete days, that would need to happen here
  
  timestamp.start = min(redistributed$timestamp) # beginning of forecast
  timestamp.end = max(redistributed$timestamp) # end of forecast
  
  ## convert longwave to hourly (just copy 6 hourly values over past 6-hour time period and remove first 6 hours to match time span of forecast)
  LongWave.hrly <- redistributed %>%
    select(timestamp, NOAA.member, LongWave) %>%
    group_by(timestamp, NOAA.member, LongWave) %>%
    expand(timestamp = c(as_datetime(timestamp - 0*60*60, tz = "US/Eastern"),
                         as_datetime(timestamp - 1*60*60, tz = "US/Eastern"),
                         as_datetime(timestamp - 2*60*60, tz = "US/Eastern"),
                         as_datetime(timestamp - 3*60*60, tz = "US/Eastern"),
                         as_datetime(timestamp - 4*60*60, tz = "US/Eastern"),
                         as_datetime(timestamp - 5*60*60, tz = "US/Eastern"))) %>%
    ungroup()
  
  ## downscale shortwave to hourly
  lat = 37.307
  lon = 360-79.837
  
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
  
  
  # -----------------------------------
  # 6. join hourly debiased forecasts
  # -----------------------------------
  
  joined.ds <- full_join(states.ds.hrly, ShortWave.ds, by = c("timestamp","NOAA.member"), suffix = c(".obs",".ds")) %>%
    full_join(LongWave.hrly, by = c("timestamp","NOAA.member"), suffix = c(".obs",".ds")) 
  return(joined.ds)
}


