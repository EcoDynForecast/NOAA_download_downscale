# --------------------------------------
# purpose: downscale GEFS forecast to specific site & hr-resolution
# Creator: Laura Puckett, December 21 2018
# contact: plaura1@vt.edu
# --------------------------------------
# summary: this function spatially downscaled forecasts from GEFS cell size to the specific site location and temporally downscaled from 6-hr resolution to hr-resolution using saved parameters from earlier fitting process (fit_downscaling_parameters.R)
# --------------------------------------

downscale_met <- function(forecasts, VarNames, VarNamesStates, USE_ENSEMBLE_MEAN, PLOT){
  # -----------------------------------
  # 0. summarize forecasts to ensemble mean if USE_ENSEMBLE_MEAN is TRUE
  # -----------------------------------
  if(USE_ENSEMBLE_MEAN){
    forecasts <- forecasts %>%
      dplyr::group_by(timestamp) %>%
      dplyr::select(-NOAA.member) %>%
      # take mean across ensembles at each timestamp
      dplyr::summarize_all("mean", na.rm = FALSE) %>%
      dplyr::mutate(NOAA.member = "mean")
  }
  
  # -----------------------------------
  # 1. aggregate forecasts and observations to daily resolution
  # -----------------------------------
  
  daily.forecast = aggregate_to_daily(forecasts)
  
  # -----------------------------------
  # 2. load saved parameters and spatially debias at daily scale
  # -----------------------------------
  
  load(file = paste(path.working,"debiased.coefficients.RData", sep = ""))
  debiased <- daily_debias_from_coeff(daily.forecast, debiased.coefficients)
  
  # -----------------------------------
  # 3.a. temporal downscaling step (a): redistribute to 6-hourly resolution
  # -----------------------------------
  redistributed = daily_to_6hr(forecasts, daily.forecast, debiased)
  
  # -----------------------------------
  # 3.b. temporal downscaling step (b): temporally downscale from 6-hourly to hourly
  # -----------------------------------
  
  ## downscale states to hourly resolution (air temperature, relative humidity, average wind speed) 
  states.ds.hrly = spline_to_hourly(redistributed)
  # if filtering out incomplete days, that would need to happen here
  
  ## convert longwave to hourly (just copy 6 hourly values over past 6-hour time period)
  LongWave.hrly <- redistributed %>%
    select(timestamp, NOAA.member, LongWave) %>%
    repeat_6hr_to_hrly()
  
  ## downscale shortwave to hourly
  ShortWave.ds = ShortWave_to_hrly(debiased, lat = 37.307, lon = 360 - 79.837)
  
  # -----------------------------------
  # 4. join debiased forecasts of different variables into one dataframe
  # -----------------------------------
  
  joined.ds <- full_join(states.ds.hrly, ShortWave.ds, by = c("timestamp","NOAA.member"), suffix = c(".obs",".ds")) %>%
    full_join(LongWave.hrly, by = c("timestamp","NOAA.member"), suffix = c(".obs",".ds")) %>%
    filter(timestamp >= min(forecasts$timestamp) & timestamp <= max(forecasts$timestamp))
  
  return(joined.ds)
  
}

