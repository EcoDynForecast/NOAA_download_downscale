# --------------------------------------
# purpose: downscale future GEFS forecasts to FCR site and hourly resolution
# Creator: Laura Puckett, December 16 2018
# contact: plaura1@vt.edu
# --------------------------------------

# -----------------------------------
# 2. change forecast units
# -----------------------------------
run_future_downscaling <- function(d, forecast.date, forecast.units.match, path.working){
  forecast.data <- d %>%
    dplyr::mutate(timestamp = as_datetime(forecast.date, tz = "US/Eastern")) %>%
    plyr::rename(c("ensembles" = "NOAA.member"))
  NOAA.na.value = 999900000000000000000
  forecast.units.match <- forecast.data %>%
    dplyr::mutate(temp = tmp2m,
                  ws = sqrt(vgrd10m^2 + ugrd10m^2),
                  avg.lw = ifelse(dlwrfsfc==NOAA.na.value, NA,dlwrfsfc),
                  avg.sw = ifelse(dswrfsfc==NOAA.na.value, NA,dswrfsfc),
                  precip.rate = ifelse( pratesfc==NOAA.na.value, NA,  pratesfc),
                  RH = rh2m) %>%
    select(NOAA.member, timestamp, temp, avg.lw, avg.sw, precip.rate, RH, ws)
  
  # -----------------------------------
  # 3. Aggregate forecast to daily resolution
  # -----------------------------------
  
  daily_forecast = forecast.units.match %>%
    dplyr::mutate(date = date(timestamp)) %>%
    group_by(NOAA.member, date) %>%
    dplyr::summarize(temp = mean(temp, na.rm = FALSE),
                     ws = mean(ws, na.rm = FALSE),
                     RH = mean(RH, na.rm = FALSE),
                     lw = mean(avg.lw, na.rm = FALSE),
                     sw = mean(avg.sw, na.rm = FALSE),
                     precip.rate = mean(precip.rate, na.rm = FALSE)) %>%
    ungroup()
  
  # need to add in statement here for if(CALCULATE_DEBIAS_COEFFICIENTS){}
  
  # -----------------------------------
  # 4. do linear debiasing at daily resolution
  # -----------------------------------
  
  debiased <- daily_debias_from_coeff(daily_forecast, debiased.coefficients)
  
  # -----------------------------------
  # 5.a. temporal downscaling step (a): redistribute to 6-hourly resolution
  # -----------------------------------
  
  NOAA.prop <- forecast.units.match %>%
    dplyr::mutate(date = date(timestamp)) %>%
    group_by(NOAA.member, date) %>%
    dplyr::mutate(temp.daily.mean = mean(temp, na.rm = FALSE),
                  RH.daily.mean = mean(RH, na.rm = FALSE),
                  ws.daily.mean = mean(ws, na.rm = FALSE),
                  lw.daily.mean = mean(avg.lw, na.rm = FALSE)) %>%
    ungroup() %>%
    mutate(temp.prop = temp - temp.daily.mean, # deviation from daily mean that each 6-hourly forecast was
           RH.prop = RH - RH.daily.mean,
           ws.prop = ws - ws.daily.mean,
           lw.prop = avg.lw - lw.daily.mean) %>%
    select(NOAA.member, date, timestamp, temp.prop, RH.prop, ws.prop, lw.prop)
  
  redistributed <- inner_join(debiased, NOAA.prop, by = c("date","NOAA.member")) %>%
    dplyr::mutate(ds.temp = temp.mod + temp.prop,
                  ds.RH = RH.mod + RH.prop,
                  ds.ws = ws.mod + ws.prop,
                  ds.lw = lw.mod + lw.prop) %>% 
    ungroup() %>%
    select(NOAA.member, timestamp, ds.temp, ds.RH, ds.ws, ds.lw)
  
  # -----------------------------------
  # 5.b. temporal downscaling step (b): temporally downscale from 6-hourly to hourly
  # -----------------------------------
  
  ## downscale states to hourly resolution (air temperature, relative humidity, average wind speed) 
  states.ds.hrly = spline_to_hourly(redistributed)
  # if filtering out incomplete days, that would need to happen here
  
  ## convert longwave to hourly (just copy 6 hourly values over past 6-hour time period)
  lw.hrly <- redistributed %>%
    select(timestamp, NOAA.member, ds.lw) %>%
    group_by(timestamp, NOAA.member, ds.lw) %>%
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
  
  sw.hours <- debiased %>%
    dplyr::group_by(NOAA.member, date) %>%
    tidyr::expand(hour = 0:23)
  
  sw.ds <- debiased %>% 
    select(sw.mod, NOAA.member, date) %>%
    dplyr::group_by(NOAA.member, date) %>%
    full_join(sw.hours, by = c("NOAA.member","date")) %>%
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
    dplyr::mutate(hrly.sw.ds = ifelse(avg.rpot > 0, sw.mod * (rpot/avg.rpot),0)) %>%
    select(timestamp, NOAA.member, hrly.sw.ds)
  
  # -----------------------------------
  # 6. join hourly observations and hourly debiased forecasts
  # -----------------------------------
  
  joined.ds <- full_join(states.ds.hrly, sw.ds, by = c("timestamp","NOAA.member")) %>%
    full_join(lw.hrly, by = c("timestamp","NOAA.member")) %>%
    plyr::rename(c("interp.temp" = "AirTemp",
                   "interp.ws" = "WindSpeed",
                   "interp.RH" = "RelHum",
                   "hrly.sw.ds" = "ShortWave",
                   "ds.lw" = "LongWave"))
  return(joined.ds)
}
#### need to determine whether or not NOAA.member should be carried through this process and fix it
