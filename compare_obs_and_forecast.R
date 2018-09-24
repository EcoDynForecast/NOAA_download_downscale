# -----------------------------------
# Script Information
# -----------------------------------
# Purpose: Make histograms of NOAA forecast and met station data
# Creator: Laura Puckett, 09-18-2018
# Contact: plaura1@vt.edu
# -----------------------------------
# Description
# -----------------------------------
# -----------------------------------
# General Workflow Components
# -----------------------------------


# --------------------------------------
# 0. Setup
# --------------------------------------

rm(list = ls())
library(lubridate)
library(dplyr)
library(tidyr)
library(plyr)
library(ggpmisc)
library(gridExtra)
path.working <- "/Users/laurapuckett/Documents/Research/Fall 2018/"
path.output <- paste(path.working, "/my_files/",sep = "")
forecasts.original <- readRDS(paste(path.working, "my_files/","NOAA.first.day.Rdata", sep = ""))
obs.original <- read.csv("/Users/laurapuckett/Documents/Research/Fall 2018/observations/carina-scc-obs/FCRmet.csv", header = TRUE)

# --------------------------------------
# 1. Align datasets (get common dates and units)
# --------------------------------------
# 

# 1.0 make date formats match. "date" is the day of the year as "YYYY-MM-DD" and "timestamp" is the date + time as "YYYY-MM-DD HH:MM:SS"

forecast.data <- forecasts.original %>%
  rename(c("forecast.date" = "date")) %>%
  dplyr::mutate(date = as_date(date),
                timestamp = as_datetime(forecast.date.hour, tz = "US/Eastern"),
                doy = lubridate::yday(date),
                hour = hour(forecast.date.hour))

obs <- obs.original %>%
  separate(TIMESTAMP, c("date","time")," ", convert = TRUE) %>%
  separate(date, c("month", "day","year"), "/") %>%
  separate(time, c("hour","minute"),":", convert = TRUE) %>%
  dplyr::mutate(month = ifelse(month < 10, paste(0, month, sep = ""), month),
                day = ifelse(day < 10, paste(0, day, sep = ""), day)) %>%
  dplyr::mutate(date = paste(year, "-", month, "-", day, sep = ""),
                doy = lubridate::yday(date)) %>%
  dplyr::mutate(timestamp = as_datetime(paste(date, " ", hour, ":", minute,":00", sep = ""), tz = "US/Eastern"),
                date = as_date(date))

# 1.1 make units match
forecast.data.mean <- forecast.data %>%
  dplyr::mutate(air_temperature = air_temperature - 273.15, # convert from K to C
                wind_speed = sqrt(eastward_wind^2 + northward_wind^2), 
                surface_downwelling_longwave_flux_in_air = ifelse(surface_downwelling_longwave_flux_in_air==999900000000000000000, NA,surface_downwelling_longwave_flux_in_air),
                surface_downwelling_shortwave_flux_in_air = ifelse(surface_downwelling_shortwave_flux_in_air==999900000000000000000, NA,surface_downwelling_shortwave_flux_in_air),
                precipitation_flux = ifelse(precipitation_flux==999900000000000000000, NA, precipitation_flux)) %>%
  group_by(timestamp) %>% # get mean across all ensembles
  dplyr::summarize(temp = mean(air_temperature, na.rm = TRUE),
                   avg.lw = mean(surface_downwelling_longwave_flux_in_air, na.rm = TRUE),
                   avg.sw = mean(surface_downwelling_shortwave_flux_in_air, na.rm = TRUE),
                   precip.rate = mean(precipitation_flux, na.rm = TRUE),
                   RH = mean(relative_humidity, na.rm = TRUE),
                   avg.ws = mean(wind_speed, na.rm = TRUE))
            
obs.units.match <- obs %>%
  dplyr::mutate(precip_rate = Rain_mm_Tot/60) # convert from mm/min to kg/m2/s
# --------------------------------------
# 1. Aggregate to 6-hourly
# --------------------------------------
# 

timestamp.0 = as_datetime("2018-04-05 08:00:00 EDT") # beginning of first 6-hour period

obs.6.hourly <- obs.units.match %>% 
  mutate(group = as.integer(difftime(timestamp,timestamp.0, units = c("mins"))/(60*6))) %>%
  group_by(group) %>% # create groups for each 6-hour time period
  dplyr::mutate(precip.rate = mean(precip_rate, na.rm = TRUE),
                avg.ws = mean(WS_ms_Avg, na.rm = TRUE),
                avg.lw = mean(IR01UpCo_Avg, na.rm = TRUE),
                avg.sw = mean(SR01Up_Avg, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(hour %in% c(2,8,14,20) & minute(timestamp) == 0) %>% 
  dplyr::rename(temp = AirTC_Avg)

joined.6.hourly = inner_join(obs.6.hourly, forecast.data.mean, by = "timestamp", suffix = c(".obs",".for"))
saveRDS(joined.6.hourly, file = paste(path.working, "my_files/","joined.6.hourly", sep = ""))

# --------------------------------------
# 1. Make Histograms and Scatterplots  Forecasts vs Obs
# --------------------------------------
# 

vars.list = c("temp","RH","avg.sw","avg.lw","precip.rate","avg.ws")
vars.title.list = c("Temperature [C]","Relative Humidity [%]","Shortwave Radiation [W/m2]","Longwave Radiation [W/m2]","Precipitation Rate [kg/m2/s]","Average Wind Speed [m/s]")

source(plot.hist.and.scatter)
source(plot.hist.difference.and.scatter)
source(check.daily.seasonal.cycles.R)
plot.hist.and.scatter(joined.6.hourly,vars.list, vars.title.list, paste(path.output, "/Hist_forecast_obs.pdf", sep = ""))
plot.hist.difference.and.scatter(joined.6.hourly,vars.list, vars.title.list, paste(path.output, "/Hist_forecast_minus_obs.pdf", sep = ""))
check.daily.seasonal.cycles(joined.6.hourly,vars.list, vars.title.list, paste(path.output, "/Timeseries.scatter.pdf", sep = ""))
