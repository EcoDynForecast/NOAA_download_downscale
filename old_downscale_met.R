# --------------------------------------
# purpose: save coefficients from linear debiasing and temporal downscaling
# Creator: Laura Puckett, December 16 2018
# contact: plaura1@vt.edu
# --------------------------------------

## setup
path.working <- "/Users/laurapuckett/Documents/Research/Fall 2018/my_files/"
setwd(path.working)
USE_ENSEMBLE_MEAN = FALSE

# -----------------------------------
# 0. Source necessary files
# -----------------------------------

source("process_saved_forecasts.R")
source("prep_obs.R")
source("prep_for.R")
source("get_daily_debias_coeff.R")
source("daily_debias_from_coeff.R")
source("spline_to_hourly.R")
source("solar_geom.R")
source("aggregate_to_daily.R")

# -----------------------------------
# 1. build dataframe of 1st day of each saved 16-day GEFS forecast file
# -----------------------------------

forecast.data.path = paste(path.working, "SCCData-noaa-data/", sep = "")
process_saved_forecasts(forecast.data.path) # geneartes flux.forecasts and state.forecasts dataframes
NOAA.flux <- readRDS(paste(path.working,"NOAA.flux.forecasts", sep = ""))
NOAA.state <- readRDS(paste(path.working,"NOAA.state.forecasts", sep = ""))
NOAA.data = inner_join(NOAA.flux, NOAA.state, by = c("forecast.date","ensembles"))

# read in obs data
obs.data <- read.csv(paste(path.working, "FCRmet.csv", sep = ""), header = TRUE)

# -----------------------------------
# 2.a prepare forecast dataset (reformat units/names and filter out days with less than 4 entries)
# -----------------------------------
ForecastVarNames = c("tmp2m",
                     "vgrd10m",
                     "ugrd10m",
                     "rh2m",
                     "dlwrfsfc",
                     "dswrfsfc")
ObsVarNames = c("AirTC_Avg",
                "WS_ms_Avg",
                "RH",
                "SR01Up_Avg",
                "IR01UpCo_Avg")
replaceObsNames = c("AirTC_Avg" = "AirTemp",
                    "WS_ms_Avg" = "WindSpeed",
                    "RH" = "RelHum",
                    "SR01Up_Avg" = "ShortWave",
                    "IR01UpCo_Avg" = "LongWave")
VarNames = c("AirTemp",
             "WindSpeed",
             "RelHum",
             "ShortWave",
             "LongWave")
VarNamesStates = c("AirTemp",
                   "WindSpeed",
                   "RelHum")

forecasts = prep_for(NOAA.data) %>%
  dplyr::group_by(NOAA.member, date(timestamp))  %>%
  dplyr::mutate(n = n()) %>%
  # force NA for days without 4 NOAA entries (because having less than 4 entries would introduce error in daily comparisons)
  dplyr::mutate_at(vars(VarNames),funs(ifelse(n == 4, ., NA))) %>%
  ungroup() %>%
  dplyr::select(-"date(timestamp)", -n)

if(USE_ENSEMBLE_MEAN){
  forecasts <- forecasts %>%
    dplyr::group_by(timestamp) %>%
    dplyr::select(-NOAA.member) %>%
    # take mean across ensembles at each timestamp
    dplyr::summarize_all("mean", na.rm = FALSE) %>%
    dplyr::mutate(NOAA.member = "mean")
}

# -----------------------------------
# 2.b prepare observations dataset (reformat units/names and filter out bad data that is sensor malfunction)
# -----------------------------------

observations = prep_obs(obs.data) %>%
  # max air temp record in Vinton, VA is 40.6 C 
  # coldest air temp on record in Vinton, Va is -23.9 C
  # http://www.climatespy.com/climate/summary/united-states/virginia/roanoke-regional 
  # lots of bad data for longwave between 8/23/2018 and 9/11/2018 randomly for a couple minutes at a       # time. Removing entire section of data for now. Also bad data on a few other days
  dplyr::mutate(AirTemp = ifelse(AirTemp> 273.15 + 41, NA, AirTemp),
                AirTemp = ifelse(AirTemp < 273.15 -23.9, NA, AirTemp),
                ShortWave = ifelse(ShortWave < 0, 0, ShortWave),
                LongWave = ifelse(LongWave < 0, NA, LongWave))

# -----------------------------------
# 3. aggregate forecasts and observations to daily resolution and join datasets
# -----------------------------------

daily.forecast = aggregate_to_daily(forecasts)

daily.obs = aggregate_to_daily(observations)
# might eventually alter this so days with at least a certain percentage of data remain in dataset (instead of becoming NA if a single minute of data is missing)

joined.data.daily <- inner_join(daily.forecast, daily.obs, by = "date", suffix = c(".for",".obs")) %>%
  ungroup

# -----------------------------------
# 4. save linearly debias coefficients and do linear debiasing at daily resolution
# -----------------------------------

debiased.coefficients <- get_daily_debias_coeff(joined.data.daily)

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

## convert longwave to hourly (just copy 6 hourly values over past 6-hour time period)
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
# 6. join debiased forecasts of different variables into one dataframe
# -----------------------------------

joined.ds <- full_join(states.ds.hrly, ShortWave.ds, by = c("timestamp","NOAA.member"), suffix = c(".obs",".ds")) %>%
  full_join(LongWave.hrly, by = c("timestamp","NOAA.member"), suffix = c(".obs",".ds")) 

# -----------------------------------
# 7. prepare dataframes of hourly observational data for comparison with forecasts
# -----------------------------------

# get hourly dataframe of shortwave and longwave observations
hrly.flux.obs <- observations %>%
  dplyr::mutate(date = date(timestamp)) %>%
  dplyr::mutate(hour = hour(timestamp)) %>%
  dplyr::group_by(date, hour) %>%
  dplyr::summarize(ShortWave = mean(ShortWave),
                   LongWave = mean(LongWave)) %>%
  ungroup() %>%
  dplyr::mutate(timestamp = as_datetime(paste(date, " ", hour, ":","00:00", sep = ""), tz = "US/Eastern") + 60*60) %>% # add one hour so that timestamp represents average over past hour
  select(timestamp, ShortWave, LongWave)


hrly.state.obs <- observations %>% group_by(timestamp) %>%
  dplyr::summarize(AirTemp = mean(AirTemp),
                   RelHum = mean(RelHum),
                   WindSpeed = mean(WindSpeed)) %>%
  ungroup()

# -----------------------------------
# 8. join hourly observations and hourly debiased forecasts
# -----------------------------------

joined.hrly.obs.and.ds <- inner_join(hrly.flux.obs, hrly.state.obs, by = "timestamp") %>%
  inner_join(joined.ds, by = "timestamp", suffix = c(".obs",".ds")) %>%

# -----------------------------------
# 9. Calculate and save coefficients from hourly downscaling (R2 and standard deviation of residuals)
# -----------------------------------

model = lm(joined.hrly.obs.and.ds$AirTemp.obs ~ joined.hrly.obs.and.ds$AirTemp.ds)
debiased.coefficients[5,1] = sd(residuals(model))
debiased.coefficients[6,1] = summary(model)$r.squared

model = lm(joined.hrly.obs.and.ds$WindSpeed.obs ~ joined.hrly.obs.and.ds$WindSpeed.ds)
debiased.coefficients[5,2] = sd(residuals(model))
debiased.coefficients[6,2] = summary(model)$r.squared

model = lm(joined.hrly.obs.and.ds$RelHum.obs ~ joined.hrly.obs.and.ds$RelHum.ds)
debiased.coefficients[5,3] = sd(residuals(model))
debiased.coefficients[6,3] = summary(model)$r.squared

model = lm(joined.hrly.obs.and.ds$ShortWave.obs ~ joined.hrly.obs.and.ds$ShortWave.ds)
debiased.coefficients[5,4] = sd(residuals(model))
debiased.coefficients[6,4] = summary(model)$r.squared

model = lm(joined.hrly.obs.and.ds$LongWave.obs ~ joined.hrly.obs.and.ds$LongWave.ds)
debiased.coefficients[5,5] = sd(residuals(model))
debiased.coefficients[6,5] = summary(model)$r.squared
save(debiased.coefficients, file = paste(path.working,"debiased.coefficients.RData", sep = ""))

# -----------------------------------
# 10. Visual check (comparing observations and downscaled forecast ensemble mean)
# -----------------------------------

ggplot(data = joined.hrly.obs.and.ds[1:5000,], aes(x = timestamp)) +
  geom_line(aes(y = AirTemp.obs, color = "observations"))+
  geom_line(aes(y = AirTemp.ds, color = "downscaled forecast average", group = NOAA.member))

ggplot(data = joined.hrly.obs.and.ds[1:5000,], aes(x = timestamp)) +
  geom_line(aes(y = RelHum.obs, color = "observations"))+
  geom_line(aes(y = RelHum.ds, color = "downscaled forecast average", group = NOAA.member))

ggplot(data = joined.hrly.obs.and.ds[1:5000,], aes(x = timestamp)) +
  geom_line(aes(y = WindSpeed.obs, color = "observations"))+
  geom_line(aes(y = WindSpeed.ds, color = "downscaled forecast average", group = NOAA.member))

ggplot(data = joined.hrly.obs.and.ds[1:5000,], aes(x = timestamp)) +
  geom_line(aes(y = ShortWave.obs, color = "observations"))+
  geom_line(aes(y = ShortWave.ds, color = "downscaled forecast average", group = NOAA.member))

ggplot(data = joined.hrly.obs.and.ds[1:5000,], aes(x = timestamp)) +
  geom_line(aes(y = LongWave.obs, color = "observations"))+
  geom_line(aes(y = LongWave.ds, color = "downscaled forecast average", group = NOAA.member))


