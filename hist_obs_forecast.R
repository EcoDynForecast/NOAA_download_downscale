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
path.working <- "/Users/laurapuckett/Documents/Research/Fall 2018/"
path.output <- paste(path.working, "/my_files/",sep = "")
forecasts.original <- readRDS(paste(path.working, "my_files/","saved.forecasts.Rdata", sep = ""))
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
                precipitation_flux = ifelse(precipitation_flux==999900000000000000000, NA, precipitation_flux),
                data.source = "forecast") %>%
  group_by(timestamp) %>% # get mean across all ensembles
  dplyr::summarize(temp = mean(air_temperature),
                   lw = mean(surface_downwelling_longwave_flux_in_air, na.rm = TRUE),
                   sw = mean(surface_downwelling_shortwave_flux_in_air, na.rm = TRUE),
                   precip.rate = mean(precipitation_flux, na.rm = TRUE),
                   RH = mean(relative_humidity, na.rm = TRUE),
                   avg.ws = mean(wind_speed, na.rm = TRUE),
                   data.source = "forecast")
            
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
                data.source = "observations") %>%
  ungroup() %>%
  filter(hour %in% c(2,8,14,20) & minute(timestamp) == 0) %>% 
  dplyr::rename(temp = AirTC_Avg,
          RH = RH,
          lw = IR01UpCo_Avg,
          sw = SR01Up_Avg)

common_cols <- intersect(colnames(obs.6.hourly), colnames(forecast.data.mean))
joined.6.hourly <- rbind(
  subset(obs.6.hourly, select = common_cols), 
  subset(forecast.data.mean, select = common_cols)
)

# --------------------------------------
# 1. Make Histograms to Compares Forecasts and Obs
# --------------------------------------
# 

## TEMPERATURE [C]

pdf(file = paste(path.output, "/Hist_comparison.pdf", sep = ""))

ggplot(data = joined.6.hourly %>% group_by(timestamp),aes(temp, fill = data.source)) + # one plot containing all ensembles
 geom_density(alpha = 0.3) +
  ggtitle("Temperature [C]")

ggplot(data = joined.6.hourly %>% group_by(timestamp),aes(RH, fill = data.source)) + # one plot containing all ensembles
  geom_density(alpha = 0.3) +
  ggtitle("Relative Humidity [%]")

ggplot(data = joined.6.hourly %>% group_by(timestamp),aes(sw, fill = data.source)) + # one plot containing all ensembles
  geom_density(alpha = 0.3) +
  ggtitle("Shortwave Radiation [W/m2]")

ggplot(data = joined.6.hourly %>% group_by(timestamp),aes(lw, fill = data.source)) + # one plot containing all ensembles
  geom_density(alpha = 0.3) +
  ggtitle("Longwave Radiation [W/m2]")

 ggplot(data = joined.6.hourly %>% group_by(timestamp), aes(precip.rate,  fill = data.source)) + # one plot containing all ensembles
  geom_density(alpha = 0.3) +
   ggtitle("Precipitation Rate [kg/m2/s]")

ggplot(data = joined.6.hourly %>% group_by(timestamp),aes(avg.ws, fill = data.source)) + # one plot containing all ensembles
  geom_density(alpha = 0.3) +
  ggtitle("Average Wind Speed [m/s]")
dev.off()
