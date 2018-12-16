# --------------------------------------
# purpose: save coefficients from linear debiasing and temporal downscaling
# Creator: Laura Puckett, December 16 2018
# contact: plaura1@vt.edu
# --------------------------------------

rm(list = ls())
## setup
path.working <- "/Users/laurapuckett/Documents/Research/Fall 2018/my_files/"
setwd(path.working)

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

# -----------------------------------
# 1. build dataframe of 1st day of each saved 16-day GEFS forecast file
# -----------------------------------

forecast.data.path = paste(path.working, "SCCData-noaa-data/", sep = "")
process_saved_forecasts(forecast.data.path) # geneartes flux.forecasts and state.forecasts dataframes
NOAA.flux <- readRDS(paste(path.working,"NOAA.flux.forecasts", sep = ""))
NOAA.state <- readRDS(paste(path.working,"NOAA.state.forecasts", sep = ""))
NOAA.data = inner_join(NOAA.flux, NOAA.state, by = c("forecast.date.hour","ensembles"))

# read in obs data
obs.data <- read.csv(paste(path.working, "FCRmet.csv", sep = ""), header = TRUE)

# -----------------------------------
# 2.a prepare forecast dataset (reformat units/names and filter out days with less than 4 entries)
# -----------------------------------

# force NA for days without 4 NOAA entries (because having less than 4 entries would introduce error in daily comparisons)
forecast.units.match = prep_for(NOAA.data) %>%
  dplyr::group_by(NOAA.member, date(timestamp))  %>%
  dplyr::mutate(n = n(),
                temp = ifelse(n == 4, temp, NA), 
                RH = ifelse(n == 4, RH, NA),
                ws = ifelse(n == 4, ws, NA),
                avg.sw = ifelse(n==4, avg.sw, NA),
                avg.lw = ifelse(n==4, avg.lw, NA),
                precip.rate = ifelse(n==4, precip.rate, NA)) %>%
  ungroup() %>%
  dplyr::group_by(timestamp) %>%
  dplyr::summarize(NOAA.member = "mean",
                   temp = mean(temp),
                   RH = mean(RH),
                   ws = mean(ws),
                   avg.sw = mean(avg.sw),
                   avg.lw = mean(avg.lw),
                   precip.rate = mean(precip.rate)) %>%
  ungroup()

# -----------------------------------
# 2.b prepare observations dataset (reformat units/names and filter out bad data that is sensor malfunction)
# -----------------------------------

obs.units.match = prep_obs(obs.data) %>%
  # max air temp record in Vinton, VA is 40.6 C 
  # coldest air temp on record in Vinton, Va is -23.9 C
  # http://www.climatespy.com/climate/summary/united-states/virginia/roanoke-regional 
  # lots of bad data for longwave between 8/23/2018 and 9/11/2018 randomly for a couple minutes at a       # time. Removing entire section of data for now. Also bad data on a few other days
  dplyr::mutate(AirTK_Avg = ifelse(AirTK_Avg > 273.15 + 41, NA, AirTK_Avg),
                AirTK_Avg = ifelse(AirTK_Avg < 273.15 -23.9, NA, AirTK_Avg),
                SR01Up_Avg = ifelse(SR01Up_Avg < 0, 0, SR01Up_Avg),
                IR01UpCo_Avg = ifelse(IR01UpCo_Avg < 0, NA, IR01UpCo_Avg)) %>%
  filter(timestamp < as_datetime("2018-11-19 00:00:00"))

# -----------------------------------
# 3. aggregate forecasts and observations to daily resolution and join datasets
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

daily_obs = obs.units.match %>%
  dplyr::mutate(date = date(timestamp)) %>%
  group_by(date) %>%
  # might eventually alter this so days with at least a certain percentage of data remain in dataset (instead of becoming NA if a single minute of data is missing)
  dplyr::summarize(temp = mean(AirTK_Avg, na.rm = TRUE),
                   ws = mean(WS_ms_Avg, na.rm = FALSE),
                   RH = mean(RH, na.rm = FALSE),
                   precip.rate = mean(precip_rate, na.rm = FALSE),
                   lw = mean(IR01UpCo_Avg, na.rm = FALSE),
                   sw = mean(SR01Up_Avg, na.rm = FALSE)) %>%
  ungroup()

joined.data.daily <- inner_join(daily_forecast, daily_obs, by = "date", suffix = c(".for",".obs")) %>%
  ungroup %>%
  filter(is.na(temp.for) == FALSE & is.na(RH.for) == FALSE && is.na(ws.for) == FALSE)

# -----------------------------------
# 4. save linearly debias coefficients and do linear debiasing at daily resolution
# -----------------------------------

debiased.coefficients <- get_daily_debias_coeff(joined.data.daily)
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
  select(NOAA.member, timestamp, dscale.member, ds.temp, ds.RH, ds.ws, ds.lw)

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
  select(sw.mod, dscale.member, NOAA.member, date) %>%
  dplyr::group_by(NOAA.member, date, dscale.member) %>%
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
  select(timestamp, hrly.sw.ds)

# -----------------------------------
# 6. prepare dataframes of hourly observational data for comparison with forecasts
# -----------------------------------

 # get hourly dataframe of shortwave and longwave observations
hrly.flux.obs <- obs.units.match %>%
  dplyr::mutate(date = date(timestamp)) %>%
  dplyr::mutate(hour = hour(timestamp)) %>%
  dplyr::group_by(date, hour) %>%
  dplyr::summarize(SR01Up_Avg = mean(SR01Up_Avg),
                   IR01UpCo_Avg = mean(IR01UpCo_Avg)) %>%
  ungroup() %>%
  dplyr::mutate(timestamp = as_datetime(paste(date, " ", hour, ":","00:00", sep = ""), tz = "US/Eastern") + 60*60) %>% # add one hour so that timestamp represents average over past hour
  select(timestamp, SR01Up_Avg, IR01UpCo_Avg)


hrly.state.obs <- obs.units.match %>% group_by(timestamp) %>%
  dplyr::summarize(AirTK_Avg = mean(AirTK_Avg),
                   RH = mean(RH),
                   WS_ms_Avg = mean(WS_ms_Avg)) %>%
  ungroup()

# -----------------------------------
# 6. join hourly observations and hourly debiased forecasts
# -----------------------------------

joined.hrly.obs.and.ds <- inner_join(hrly.flux.obs, hrly.state.obs, by = "timestamp") %>%
  inner_join(states.ds.hrly, by = "timestamp") %>%
  inner_join(sw.ds, by = "timestamp") %>%
  inner_join(lw.hrly, by = c("timestamp","NOAA.member"))

#### need to determine whether or not NOAA member should be carried through this process and fix it

# -----------------------------------
# 7. Calculate and save coefficients from hourly downscaling (R2 and standard deviation of residuals)
# -----------------------------------

model = lm(joined.hrly.obs.and.ds$AirTK_Avg ~ joined.hrly.obs.and.ds$interp.temp)
debiased.coefficients[5,1] = sd(residuals(model))
debiased.coefficients[6,1] = summary(model)$r.squared

model = lm(joined.hrly.obs.and.ds$RH ~ joined.hrly.obs.and.ds$interp.RH)
debiased.coefficients[5,2] = sd(residuals(model))
debiased.coefficients[6,2] = summary(model)$r.squared

model = lm(joined.hrly.obs.and.ds$WS_ms_Avg ~ joined.hrly.obs.and.ds$interp.ws)
debiased.coefficients[5,3] = sd(residuals(model))
debiased.coefficients[6,3] = summary(model)$r.squared

model = lm(joined.hrly.obs.and.ds$SR01Up_Avg ~ joined.hrly.obs.and.ds$hrly.sw.ds)
debiased.coefficients[5,4] = sd(residuals(model))
debiased.coefficients[6,4] = summary(model)$r.squared

model = lm(joined.hrly.obs.and.ds$IR01UpCo_Avg ~ joined.hrly.obs.and.ds$ds.lw)
debiased.coefficients[5,5] = sd(residuals(model))
debiased.coefficients[6,5] = summary(model)$r.squared
save(debiased.coefficients, file = paste(path.working,"debiased.coefficients.RData", sep = ""))

# -----------------------------------
# 7. Visual check (comparing observations and downscaled forecast ensemble mean)
# -----------------------------------

ggplot(data = joined.hrly.obs.and.ds[1:500,], aes(x = timestamp)) +
  geom_line(aes(y = AirTK_Avg, color = "observations"))+
  geom_line(aes(y = interp.temp, color = "downscaled forecast average"))

ggplot(data = joined.hrly.obs.and.ds[1:500,], aes(x = timestamp)) +
  geom_line(aes(y = RH, color = "observations"))+
  geom_line(aes(y = interp.RH, color = "downscaled forecast average"))

ggplot(data = joined.hrly.obs.and.ds[1:500,], aes(x = timestamp)) +
  geom_line(aes(y = WS_ms_Avg, color = "observations"))+
  geom_line(aes(y = interp.ws, color = "downscaled forecast average"))

ggplot(data = joined.hrly.obs.and.ds[1:500,], aes(x = timestamp)) +
  geom_line(aes(y = SR01Up_Avg, color = "observations"))+
  geom_line(aes(y = hrly.sw.ds, color = "downscaled forecast average"))

ggplot(data = joined.hrly.obs.and.ds[1:500,], aes(x = timestamp)) +
  geom_line(aes(y = IR01UpCo_Avg, color = "observations"))+
  geom_line(aes(y = ds.lw, color = "downscaled forecast average"))
  

