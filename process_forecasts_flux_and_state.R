# -----------------------------------
# Script Information
# -----------------------------------
# Purpose: Combine saved NOAA forecasts into dataframes
# Creator: Laura Puckett, September 5 2018
# Contact: plaura1@vt.edu
# -----------------------------------
# Description
# -----------------------------------
# Take the first day of each forecast and combine into dataframes by ensemble member
# -----------------------------------
# General Workflow Components
# -----------------------------------
# 0. load packages, initialize variables, naviage to data
# 1. 
#    
# 2. 
# -----------------------------------
# TO DO:
# should change to get files directly from github
# clean up and comment
# 
#
# 0. load packages, initialize variables, naviage to data
# 
rm(list = ls())
library(lubridate)
library(dplyr)
data.path = "/Users/laurapuckett/Documents/Research/Fall 2018/my_files/SCCData-noaa-data/"
forecast.files.list = list.files(data.path)
num.ensembles = 21
st <- as.Date("2018-04-23") # start of available data
en <- as.Date(Sys.Date()) 
date.list <- seq(st, en, by = "1 day")
date.list <- force_tz(as_datetime(date.list), "EST")
flux.forecasts = NULL
state.forecasts = NULL
path.working <- "/Users/laurapuckett/Documents/Research/Fall 2018/my_files/"

# 1. pull first day of forecasts from all dates unless data is missing for a day, then that date's forecast from the most recent available data
#
for(i in 1:length(date.list)){
  date.path = NULL
  temp.data = NULL
  temp.year = NULL
  temp.month = NULL
  temp.day = NULL
  temp.year = lubridate::year(date.list[i])
  temp.month = lubridate::month(date.list[i])
  if(temp.month<10){temp.month = paste("0",temp.month, sep = "")}
  temp.day = lubridate::day(date.list[i])
  if(temp.day<10){temp.day = paste("0",temp.day, sep = "")}
  date.path = paste(temp.year,temp.month,temp.day, sep = "")
  
  if(paste(date.path,"gep_all_00z.csv", sep = "")%in% forecast.files.list){
    full.path = paste(data.path, date.path,"gep_all_00z.csv", sep = "")
    tmp.data = read.csv(full.path) %>% 
      mutate(forecast.date.hour = force_tz(as.POSIXct(strptime(forecast.date, "%Y-%m-%d %H:%M:%S")), "EST"), # date including hour
             # forecast.date = lubridate::as_date(forecast.date), # doesn't include hour, only date
             NOAA.file.group = i) # group number for which file the data is from
    
    tmp.state <- tmp.data %>%
      filter(forecast.date.hour %in% c(date.list[i] - hours(4), date.list[i] - hours(3), date.list[i] - hours(5),
                                       date.list[i] + hours(1), date.list[i] + hours(2), date.list[i] + hours(0),
                                       date.list[i] + hours(8), date.list[i] + hours(9), date.list[i] + hours(7),
                                       date.list[i] + hours(14), date.list[i] + hours(15), date.list[i] + hours(13))) %>%
      select(ensembles, tmp2m, rh2m, vgrd10m, ugrd10m, forecast.date.hour, NOAA.file.group)
    
    tmp.flux <- tmp.data %>%
      filter(forecast.date.hour %in% c(date.list[i] + hours(2),date.list[i] + hours(1),
                                       date.list[i] + hours(8), date.list[i] + hours(7),
                                       date.list[i] + hours(14), date.list[i] + hours(13),
                                       date.list[i] + hours(20),date.list[i] + hours(19))) %>%
      select(ensembles, pratesfc, dlwrfsfc, dswrfsfc, forecast.date.hour, NOAA.file.group)
    flux.forecasts = rbind(flux.forecasts, tmp.flux)
    state.forecasts = rbind(state.forecasts, tmp.state)
  }else{
    print(paste("Missing a file for date: ", date.path, sep = ""))
  }
  
}
library("plyr")
flux.forecasts <- flux.forecasts  %>%
  rename(c("dlwrfsfc" = "surface_downwelling_longwave_flux_in_air",
           "dswrfsfc" = "surface_downwelling_shortwave_flux_in_air",
           "pratesfc" = "precipitation_flux"))
state.forecasts <- state.forecasts %>%
  rename(c("tmp2m" = "air_temperature",
           "rh2m" = "relative_humidity",
           "vgrd10m" = "northward_wind",
           "ugrd10m" = "eastward_wind"))
saveRDS(flux.forecasts, file = paste(path.working,"NOAA.flux.forecasts", sep = ""))
saveRDS(state.forecasts, file = paste(path.working,"NOAA.state.forecasts", sep = ""))


