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
                IR01UpCo_Avg = ifelse(IR01UpCo_Avg <0, NA, IR01UpCo_Avg)) %>%
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





