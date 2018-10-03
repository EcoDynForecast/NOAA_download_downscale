rm(list = ls())
### THIS PROCESS DOESN'T WORK FOR TIMES WITH MISSING DATA
library(lubridate)
library(dplyr)
library(plyr)
library(tidyr)
library(ggpmisc)
library(gridExtra)
library(purrr)

## solar geometry
inv =  1/24
start_day = 115
num_days = 5
end_day = start_day + num_days
doy = seq(start_day,end_day, by = inv)
lat = 37.307
lon = 360-79.837
solar_geom(doy = seq(start_day,end_day, by = inv), lon, lat)
# 9/29 times: 9:25-10:00 & 10:50-11:20
# 10/01 times: 7:00-10:30 figuring out code from Mike Dietz, spline function, purrr, nested dataframes, figuring out bug in code (was actually caused by gaps in data)
# tasks:

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
  group_by(ensembles, avg.sw, doy) %>%
  tidyr::expand(doy = c(doy - 6/24,doy - 5/24,doy - 3/24,doy - 2/24,doy - 1/24, doy)) # repeat NOAA values over past 6 hours

tmp.obs <- obs %>% filter(doy >= start_day & doy <= end_day) %>% group_by(timestamp, doy, hour)
tmp.NOAA <- forecast.obs.repeat %>% filter(doy >= start_day & doy <= end_day)
tmp.mod = data.frame(doy = seq(start_day,end_day, by = inv), solar = solar_geom(seq(start_day,end_day, by = inv)+4/24, lon, lat))

ggplot() +
  geom_line(data =tmp.mod, aes(doy, solar), col = "red")+
  geom_line(data = tmp.obs, aes(doy, SR01Up_Avg), col = "black")+
  geom_line(data = tmp.NOAA, aes(doy, avg.sw, group = ensembles), col = "blue") +
scale_color_manual(values= colors, labels = c("four", "six", "eight")) 

colors <- c(
  "solar_geom()" = "red",
  "obs" = "black",
  "NOAA" = "blue"
)


plot(doy, solar_geom(doy = doy+4/24, lon, lat), type = "l", col = 'red', xlab = "day of year", ylab = "W/m2")
lines(tmp.obs$doy, tmp.obs$SR01Up_Avg, type = 'l')
points(tmp.NOAA$doy, tmp.NOAA$avg.sw, col = 'blue', type = 'l')

### interpolating sw

interpolate <- function(doy, var){
  result <- splinefun(doy, var, method = "monoH.FC")
  return(result(seq(min(doy),max(doy),1/48)))
}

by.ens <- forecast.units.match %>% 
  group_by(ensembles)

new.dfs.vars <- do(by.ens, interp.sw = interpolate(.$doy,.$avg.sw))
new.dfs.doy <- by.ens %>% do(doy = seq(min(forecast.units.match$doy), max(forecast.units.match$doy), 1/48))
new.dfs <- inner_join(new.dfs.doy, new.dfs.vars, by = "ensembles") %>% unnest()

tmp.new.dfs <- new.dfs %>% filter(doy >= start_day & doy <= end_day)
tmp.new.dfs <- tmp.new.dfs %>%
  mutate(doy = doy - 3/24,
         interp.sw = ifelse(interp.sw<0,0,interp.sw)) # adjust to be center of 6-hour period and force values >=0
alpha = 0.7
ggplot() +
  geom_line(data =tmp.mod, aes(doy, solar), col = "red", alpha = alpha)+
  geom_line(data = tmp.obs, aes(doy, SR01Up_Avg), col = "black", alpha = alpha)+
  geom_line(data = tmp.NOAA, aes(doy, avg.sw, group = ensembles), col = "blue", alpha = alpha) +
  geom_line(data = tmp.new.dfs, aes(doy, interp.sw, group = ensembles), col = "green", alpha = alpha)
  scale_color_manual(values= colors, labels = c("four", "six", "eight")) 
