# -----------------------------------
# Script Information
# -----------------------------------
# Purpose: Compare saved forecasts and observational measurements of meteorological data at the SCC site
# Creator: Laura Puckett, 09-16-2018
# Contact: plaura1@vt.edu
# -----------------------------------
# Description
# -----------------------------------
# Convert forcast and observational data to common format and compare at the 6-hourly scale
# -----------------------------------
# General Workflow Components
# -----------------------------------
# 0. Set up file structure, load packages, etc
# 1. Get common date & units between datasets (need to finish on common units)
# 2. Aggregate to daily res and compare
# 3. Compare 6-hourly and generate pdfs of plots (temperature, relative humidity, wind speed)
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
  mutate(date = as_date(date),
         timestamp = as_datetime(forecast.date.hour, tz = "US/Eastern"))

obs <- obs.original %>%
  separate(TIMESTAMP, c("date","time")," ") %>%
  separate(date, c("month", "day","year"), "/") %>%
  mutate(month = as.integer(month),
         day = as.integer(day),
         year = as.integer(year),
         time = paste(time, ":00", sep = "")) %>%
  mutate(month = ifelse(month < 10, paste(0, month, sep = ""), month),
         day = ifelse(day < 10, paste(0, day, sep = ""), day)) %>%
  mutate(date = paste(year, "-", month, "-", day, sep = "")) %>%
  mutate(timestamp = as_datetime(paste(date, " ", time, sep = ""), tz = "US/Eastern"),
         date = as_date(date))

# 1.1 make units match

forecast.data <- forecast.data %>%
  mutate(air_temperature = air_temperature - 273.15,
         wind_speed = sqrt(eastward_wind^2 + northward_wind^2))
 # obs, need to :convert precipitation from tot mm to kg/m^2/s 
 #  rename columns to match forecast.data names

#-------------------------------
# 2. Aggregate temperature to daily and compare
# -------------------------------

obs.daily <- obs %>% 
  group_by(date) %>% 
  dplyr::summarize(obs.daily.temp = mean(AirTC_Avg, na.rm = TRUE))

forecast.daily <- forecast.data %>%
  group_by(date, ensembles) %>%
  dplyr::summarize(forecast.daily.temp = mean(air_temperature, na.rm = TRUE))

joined.daily <- dplyr::inner_join(obs.daily, forecast.daily, by = "date")

for(i in 1:21){ # one plot for each ensemble
  p <- ggplot() +
    geom_point(data = filter(joined.daily, ensembles == i), aes(obs.daily.temp, forecast.daily.temp)) + 
    geom_abline(slope = 1, intercept = 0)
  print(p)
}
ggplot() + # one plot containing all ensembles
geom_point(data = joined.daily, aes(obs.daily.temp, forecast.daily.temp)) + 
  geom_abline(slope = 1, intercept = 0)
  
# -------------------------------
# 3. Compare 6-hourly forecast data with same time for 1-hourly observational data (need to aggregate precip later)
# -------------------------------

obs.hourly <- obs %>% 
  group_by(timestamp)

forecast.6.hourly <- forecast.data %>%
  group_by(timestamp, ensembles) 

joined.hourly <- dplyr::inner_join(obs.hourly, forecast.6.hourly, by = "timestamp")


## TEMPERATURE [C]

my.formula <- y ~ x
pdf(file = paste(path.output, "/Temp_comparison.pdf", sep = ""))
ggplot(data = joined.hourly, aes(AirTC_Avg, air_temperature)) + # one plot containing all ensembles
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, col = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlab("obs") + 
  ylab("forecast") +
  ggtitle("Temperature [C] all ensembles")

for(i in 1:21){ # one plot for each ensemble
  p <- ggplot(data = filter(joined.hourly, ensembles == i), aes(AirTC_Avg, air_temperature)) +
    geom_point() + 
    geom_abline(slope = 1, intercept = 0, col = "red") +
    geom_smooth(method = "lm", se = FALSE, color = "blue", formula = my.formula) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE) +
    xlab("obs") + 
    ylab("forecast") +
    ggtitle(paste("Temperature [C], ensemble", i))
  print(p)
}
dev.off()

## RELATIVE HUMIDITY [%]
pdf(file = paste(path.output, "/RH_comparison.pdf", sep = ""))
ggplot(data = joined.hourly, aes(RH, relative_humidity)) + # one plot containing all ensembles
geom_point() + 
  geom_abline(slope = 1, intercept = 0, col = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlab("obs") + 
  ylab("forecast") +
  ggtitle("Relative Humidity [%] all ensembles")

for(i in 1:21){ # one plot for each ensemble
  p <- ggplot(data = filter(joined.hourly, ensembles == i), aes(RH, relative_humidity)) +
    geom_point() + 
    geom_abline(slope = 1, intercept = 0, col = "red") +
    geom_smooth(method = "lm", se = FALSE, color = "blue", formula = my.formula) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE) +
    xlab("obs") + 
    ylab("forecast") +
    ggtitle(paste("Relative Humidity [%], ensemble",i))
  print(p)
}
dev.off()

## Wind Speed [m/s]

pdf(file = paste(path.output, "/WS_comparison.pdf", sep = ""))
ggplot(data = joined.hourly,  aes(WS_ms_Avg, wind_speed)) + # one plot containing all ensembles
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, col = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlab("obs") + 
  ylab("forecast") +
  ggtitle("Wind Speed [m/s] all ensembles")

for(i in 1:21){ # one plot for each ensemble
  p <- ggplot(data = filter(joined.hourly, ensembles == i), aes(WS_ms_Avg, wind_speed)) +
    geom_point() + 
    geom_abline(slope = 1, intercept = 0, col = "red") +
    geom_smooth(method = "lm", se = FALSE, color = "blue", formula = my.formula) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE) +
    xlab("obs") + 
    ylab("forecast") +
    ggtitle(paste("Wind Speed [m/s], ensemble",i))
  print(p)
}
dev.off()