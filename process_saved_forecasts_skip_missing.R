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
# need to split by ensemble
# make column to say which date the data is coming from
# clean up and comment
# 

#
# 0. load packages, initialize variables, naviage to data
# 
rm(list = ls())
library(lubridate)
library(dplyr)
data.path = "/Users/laurapuckett/Documents/Research/Fall 2018/SCCData-noaa-data/"
forecast.files.list = list.files(data.path)
num.ensembles = 21
st <- as.Date("2018-04-22") # start of available data
en <- as.Date(Sys.Date()) 
date.list <- seq(st, en, by = "1 day")
forecasts.combined = NULL
path.working <- "/Users/laurapuckett/Documents/Research/Fall 2018/"

# 1. pull first day of forecasts from all dates unless data is missing for a day, then that date's forecast from the most recent available data
#
for(i in 1:length(date.list)){
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
    full.path = paste("/Users/laurapuckett/Documents/Research/Fall 2018/SCCData-noaa-data/",date.path,"gep_all_00z.csv", sep = "")
    temp.data = read.csv(full.path) %>% 
      mutate(forecast.date.hour = as.POSIXct(strptime(forecast.date, "%Y-%m-%d %H:%M:%S")), # date including hour
             forecast.date = lubridate::as_date(forecast.date)) %>% # doesn't include hour, only date
      filter(forecast.date == date.list[i])
  }else{
    print(paste("You are missing a file for date: ", date.path, sep = ""))
  }
  forecasts.combined = rbind(forecasts.combined, temp.data)
  
}

files.train <- forecasts.combined %>%
  rename(c("tmp2m" = "air_temperature",
           "dlwrfsfc" = "surface_downwelling_longwave_flux_in_air",
           "dswrfsfc" = "surface_downwelling_shortwave_flux_in_air",
           "pratesfc" = "precipitation_flux",
           "rh2m" = "relative_humidity",
           "vgrd10m" = "northward_wind",
           "ugrd10m" = "eastward_wind"))
saveRDS(files.train, file = paste(path.working, "my_files/","NOAA.first.day.Rdata", sep = ""))


