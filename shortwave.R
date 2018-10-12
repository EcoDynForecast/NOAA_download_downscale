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
  dplyr::mutate(date = as.Date(TIMESTAMP, format = '%m/%d/%y'), tz = "US/Eastern") %>%
  separate(TIMESTAMP, c("date.extra","time")," ", convert = TRUE) %>%
  dplyr::mutate(yday = lubridate::yday(date)) %>%
  separate(time, c("hour","minute"),":", convert = TRUE) %>%
  dplyr::mutate(timestamp = as_datetime(paste(date, " ", hour, ":", minute,":00", sep = ""), tz = "US/Eastern"))
attributes(obs$timestamp)$tzone <- "EST"
obs <- obs %>% dplyr::mutate(date = as_date(date),
                      doy = yday(timestamp) + hour(timestamp)/24 + minute(timestamp)/(24*60))
# 1.1 make units match
path.working <- "/Users/laurapuckett/Documents/Research/Fall 2018/"
NOAA.data <- readRDS(paste(path.working, "my_files/","NOAA.first.day.Rdata", sep = ""))
forecast.data <- NOAA.data %>%
  rename(c("forecast.date" = "date")) %>%
  dplyr::mutate(date = as_date(date),
                timestamp = as_datetime(forecast.date.hour, tz = "US/Eastern"))
attributes(forecast.data$timestamp)$tzone <- "EST"
forecast.data <- forecast.data %>% dplyr::mutate(yday = lubridate::yday(date),
                hour = hour(timestamp),
                doy = yday(timestamp) + hour(timestamp)/24)

forecast.units.match <- forecast.data %>%
  dplyr::mutate(air_temperature = air_temperature - 273.15, # convert from K to C
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
for.ds <- forecast.units.match

obs.units.match <- obs %>%
  dplyr::mutate(precip_rate = Rain_mm_Tot/60)
forecast.obs.repeat <- forecast.units.match %>%
  dplyr::group_by(ensembles, avg.sw, doy) %>%
  tidyr::expand(doy = c(doy - 5/24,doy - 4/24,doy - 3/24,doy - 2/24,doy - 1/24, doy)) # repeat NOAA values over past 6 hours

## solar geometry
inv =  1/24
start_day = 140
num_days = 5
end_day = start_day + num_days
doy = seq(start_day,end_day, by = inv)
lat = 37.307
lon = 360-79.837
solar_geom(doy = seq(start_day,end_day, by = inv), lon, lat)

### temporally downscale sw
for.ds[,"group.num"] = row(for.ds)[,1] # create a group number for each original NOAA forecast entry
for.ds.expanded <- for.ds %>% group_by(group.num, avg.sw, date, hour, ensembles) %>%
  expand(doy = c(doy - 6/24,doy - 5/24,doy - 4/24, doy - 3/24,doy - 2/24,doy - 1/24, doy)) %>%
  ungroup()# add times for past 6 hours (corresponding to each NOAA entry)

sw.ds <- for.ds.expanded %>%
  dplyr::group_by(ensembles) %>%
  mutate(rpot = solar_geom(.$doy+4/24, lon, lat)) %>%
  dplyr::group_by(group.num) %>%
  dplyr::mutate(avg.rpot = mean(rpot)) %>%
  ungroup() %>%
  dplyr::mutate(rpot.adj = ifelse(avg.rpot >0,rpot * (avg.sw/avg.rpot),0))


tmp.sw.ds <- sw.ds %>% filter(doy >= start_day & doy <= end_day)
tmp.obs <- obs %>% filter(doy >= start_day & doy <= end_day) %>% group_by(timestamp, doy, hour)
tmp.NOAA <- forecast.obs.repeat %>% filter(doy >= start_day & doy <= end_day)
tmp.mod = data.frame(doy = seq(start_day,end_day, by = inv), solar = solar_geom(seq(start_day,end_day, by = inv)+4/24, lon, lat))



alpha = 0.7
ggplot() +
  geom_line(data =tmp.mod, aes(doy, solar), col = "red", alpha = alpha)+
  geom_line(data = tmp.obs, aes(doy, SR01Up_Avg), col = "black", alpha = alpha)+
  geom_line(data = tmp.NOAA, aes(doy, avg.sw, group = ensembles), col = "blue", alpha = alpha) +
  geom_line(data = tmp.sw.ds, aes(doy, rpot.adj, group = ensembles), col = "green", alpha = alpha)
