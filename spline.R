rm(list = ls())
### THIS PROCESS DOESN'T WORK FOR TIMES WITH MISSING DATA
library(lubridate)
library(dplyr)
library(plyr)
library(tidyr)
library(ggpmisc)
library(gridExtra)
library(purrr)

## Organizing forecast and observational Data
obs.data <- read.csv("/Users/laurapuckett/Documents/Research/Fall 2018/observations/carina-scc-obs/FCRmet.csv", header = TRUE)

obs <- obs.data %>%
  mutate(date = as.Date(TIMESTAMP, format = '%m/%d/%y'), tz = "US/Eastern") %>%
  separate(TIMESTAMP, c("date.extra","time")," ", convert = TRUE) %>%
  mutate(yday = lubridate::yday(date)) %>%
  separate(time, c("hour","minute"),":", convert = TRUE) %>%
  dplyr::mutate(timestamp = as_datetime(paste(date, " ", hour, ":", minute,":00", sep = ""), tz = "US/Eastern"))
attributes(obs$timestamp)$tzone <- "EST"
obs <- obs %>% mutate(date = as_date(date),
                      doy = yday(timestamp) + hour(timestamp)/24 + minute(timestamp)/(24*60))
# 1.1 make units match
path.working <- "/Users/laurapuckett/Documents/Research/Fall 2018/"
NOAA.data <- readRDS(paste(path.working, "my_files/","NOAA.first.day.Rdata", sep = ""))
forecast.data <- NOAA.data %>%
  rename(c("forecast.date" = "date")) %>%
  dplyr::mutate(date = as_date(date),
                timestamp = as_datetime(forecast.date.hour, tz = "US/Eastern"))
attributes(forecast.data$timestamp)$tzone <- "EST"
forecast.data <- forecast.data %>% mutate(yday = lubridate::yday(date),
                hour = hour(timestamp),
                doy = yday(timestamp) + hour(timestamp)/24)

forecast.units.match <- forecast.data %>%
  mutate(air_temperature = air_temperature - 273.15, # convert from K to C
         wind_speed = sqrt(eastward_wind^2 + northward_wind^2), 
         surface_downwelling_longwave_flux_in_air = ifelse(surface_downwelling_longwave_flux_in_air==999900000000000000000, NA,surface_downwelling_longwave_flux_in_air),
         surface_downwelling_shortwave_flux_in_air = ifelse(surface_downwelling_shortwave_flux_in_air==999900000000000000000, NA,surface_downwelling_shortwave_flux_in_air),
         precipitation_flux = ifelse(precipitation_flux==999900000000000000000, NA, precipitation_flux)) %>%
  rename(c("air_temperature" = "temp",
           "surface_downwelling_longwave_flux_in_air" = "avg.lw",
           "surface_downwelling_shortwave_flux_in_air" = "avg.sw",
           "precipitation_flux" = "precip.rate",
           "relative_humidity" = "RH",
           "wind_speed" = "avg.ws"))

obs.units.match <- obs %>%
  dplyr::mutate(precip_rate = Rain_mm_Tot/60)
forecast.obs.repeat <- forecast.units.match %>%
  group_by(ensembles, temp, avg.ws, RH, doy) %>%
  tidyr::expand(doy = c(doy - 6/24,doy - 5/24,doy - 4/24,doy - 3/24,doy - 2/24,doy - 1/24, doy)) # repeat NOAA values over past 6 hours

start_day = 140
num_days = 5
end_day = start_day + num_days

tmp.obs <- obs %>% filter(doy >= start_day & doy <= end_day) %>% group_by(timestamp, doy, hour)
tmp.NOAA <- forecast.obs.repeat %>% filter(doy >= start_day & doy <= end_day)

ggplot() +
  geom_line(data = tmp.obs, aes(doy, SR01Up_Avg), col = "black")+
  geom_line(data = tmp.NOAA, aes(doy, avg.sw, group = ensembles), col = "blue") +
scale_color_manual(values= colors, labels = c("four", "six", "eight")) 

colors <- c(
  "solar_geom()" = "red",
  "obs" = "black",
  "NOAA" = "blue"
)

### interpolating sw

interpolate <- function(doy, var){
  result <- splinefun(doy, var, method = "monoH.FC")
  return(result(seq(min(doy),max(doy),1/24)))
}

by.ens <- forecast.units.match %>% 
  group_by(ensembles)

new.dfs.temp <- do(by.ens, interp.temp = interpolate(.$doy,.$temp))
new.dfs.avg.ws <- do(by.ens, interp.avg.ws = interpolate(.$doy,.$avg.ws))
new.dfs.RH <- do(by.ens, interp.RH = interpolate(.$doy,.$RH))
new.dfs.doy <- by.ens %>% do(doy = seq(min(forecast.units.match$doy), max(forecast.units.match$doy), 1/24))
new.dfs <- inner_join(new.dfs.doy, new.dfs.temp, by = "ensembles") %>%
  inner_join(new.dfs.avg.ws) %>%
  inner_join(new.dfs.RH) %>% unnest()

tmp.new.dfs <- new.dfs %>% filter(doy >= start_day & doy <= end_day)
tmp.new.dfs <- tmp.new.dfs %>%
  mutate(doy = doy) # adjust to be center of 6-hour period and force values >=0
alpha = 0.7
## temp
ggplot() +
  geom_line(data = tmp.obs, aes(doy, AirTC_Avg), col = "black", alpha = alpha)+
  geom_line(data = tmp.NOAA, aes(doy, temp, group = ensembles), col = "blue", alpha = alpha) +
  geom_line(data = tmp.new.dfs, aes(doy, interp.temp, group = ensembles), col = "green", alpha = alpha)
  scale_color_manual(values= colors, labels = c("four", "six", "eight")) 
## wind speed  
  ggplot() +
    geom_line(data = tmp.obs, aes(doy, WS_ms_Avg), col = "black", alpha = alpha)+
    geom_line(data = tmp.NOAA, aes(doy, avg.ws, group = ensembles), col = "blue", alpha = alpha) +
    geom_line(data = tmp.new.dfs, aes(doy, interp.avg.ws, group = ensembles), col = "green", alpha = alpha)
  scale_color_manual(values= colors, labels = c("four", "six", "eight")) 
## relative humidity  
  ggplot() +
    geom_line(data = tmp.obs, aes(doy, RH), col = "black", alpha = alpha) +
    geom_line(data = tmp.NOAA, aes(doy, RH, group = ensembles), col = "blue", alpha = alpha) +
    geom_line(data = tmp.new.dfs, aes(doy, interp.RH, group = ensembles), col = "green", alpha = alpha)
  scale_color_manual(values= colors, labels = c("four", "six", "eight")) 

  