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
library(lubridate)
library(dplyer)
data.path = "/Users/laurapuckett/Documents/Research/Fall 2018/SCCData-noaa-data/"
forecast.files.list = list.files(data.path)
num.ensembles = 21
st <- as.Date("2018-04-22") # start of available data
en <- as.Date(Sys.Date()) 
date.list <- seq(st, en, by = "1 day")
forecasts.combined = NULL
path.working <- "/Users/laurapuckett/Documents/Research/Fall 2018/"
site.name = "FALLINGCREEK"
outfolder <- file.path(path.working, "/saved_forecasts_as_nc/", site.name, "NLDAS_day")
dir.create(outfolder, recursive=T)

# 1. pull first day of forecasts from all dates unless data is missing for a day, then that date's forecast from the most recent available data
#
for(i in 1:length(date.list)){
  temp.year = lubridate::year(date.list[i])
  temp.month = lubridate::month(date.list[i])
  if(temp.month<10){temp.month = paste("0",temp.month, sep = "")}
  temp.day = lubridate::day(date.list[i])
  if(temp.day<10){temp.day = paste("0",temp.day, sep = "")}
  date.path = paste(temp.year,temp.month,temp.day, sep = "")
  
  if(paste(date.path,"gep_all_00z.csv", sep = "")%in% forecast.files.list){
    full.path = paste("/Users/laurapuckett/Documents/Research/Fall 2018/SCCData-noaa-data/",date.path,"gep_all_00z.csv", sep = "")
    last.data = date.list[i]
  }else if(paste(date.path,"gep_all_06z.csv", sep = "")%in% forecast.files.list){
    full.path = paste("/Users/laurapuckett/Documents/Research/Fall 2018/SCCData-noaa-data/",date.path,"gep_all_06z.csv", sep = "")
    last.data = date.list[i]
    last.path = full.path
  }else if(paste(date.path,"gep_all_12z.csv", sep = "")%in% forecast.files.list){
    full.path = paste("/Users/laurapuckett/Documents/Research/Fall 2018/SCCData-noaa-data/",date.path,"gep_all_12z.csv", sep = "")
    last.data = date.list[i]
    last.path = full.path
  }else if(paste(date.path,"gep_all_18z.csv", sep = "")%in% forecast.files.list){
    full.path = paste("/Users/laurapuckett/Documents/Research/Fall 2018/SCCData-noaa-data/",date.path,"gep_all_18z.csv", sep = "")
    last.data = date.list[i]
    last.path = full.path
  }else{
    print(paste("You are missing a file for date: ", date.path, sep = ""))
    diff.day = date.list[i] - last.data
    full.path = last.path
  }
  
  temp.data = read.csv(full.path) %>% 
    mutate(forecast.date.hour = as.POSIXct(strptime(forecast.date, "%Y-%m-%d %H:%M:%S")), # date including hour
           forecast.date = lubridate::as_date(forecast.date)) %>% # doesn't include hour, only date
    filter(forecast.date == date.list[i])
  forecasts.combined = rbind(forecasts.combined, temp.data)
  temp.year = NULL
  temp.month = NULL
  temp.day = NULL
  temp.data = NULL
  full.path = NULL
}
#
# # 2. Make array for each ensemble member
# # 
# for(i in 1:num.ensembles){
#   assign(paste("",i,"forecasts", sep = ""), filter(forecasts_combined,ensembles == i))
# }

files.train = forecasts.combined %>%
  rename(c("tmp2m" = "air_temperature",
           "dlwrfsfc" = "surface_downwelling_longwave_flux_in_air",
           "dswrfsfc" = "surface_downwelling_shortwave_flux_in_air",
           "pratesfc" = "precipitation_flux",
           "rh2m" = "relative_humidity",
           "vgrd10m" = "northward_wind",
           "ugrd10m" = "eastward_wind"))

# for(i in 1:length(files.train)){

# Figure out what year we're working with
#yr.now <- as.numeric(strsplit(files.train[i], "[.]")[[1]][2])
yr.now = lubridate::year(files.train$forecast.date[1])
nday <- ifelse(leap_year(yr.now), 366, 365)

# dat.day <- list()

# Open the file so we can query from it
#ncT <- nc_open(file.path(path.ldas, files.train[i]))

# Extract som plot dimensions
#lat.nc <- ncvar_get(ncT, "latitude")
#lon.nc <- ncvar_get(ncT, "longitude")
lat.nc  =  37.306446
lon.nc  = -79.837585 

#time.nc <- ncvar_get(ncT, "time") 
# time.day <- apply(matrix(time.nc, ncol=nday), 2, mean) # get the daily time stamps
time.day <- as.numeric(paste(lubridate::year(files.train$forecast.date),ifelse(lubridate::month(files.train$forecast.date)<10,paste("0",lubridate::month(files.train$forecast.date),sep = ""),lubridate::month(files.train$forecast.date)),ifelse(lubridate::day(files.train$forecast.date)<10,paste("0",lubridate::day(files.train$forecast.date),sep = ""),lubridate::day(files.train$forecast.date)), sep = ""))


if("air_temperature" %in% colnames(files.train)){
  files.train <- files.train %>% group_by(forecast.date) %>%
    mutate(air_temperature_minimum = min(air_temperature),
           air_temperature_maximum = max(air_temperature)) %>%
    ungroup()
}else if(c("eastward_wind", "northward_wind") %in% colnames(files.train)){
  files.train <- group_by(forecast.date) %>%
    mutate(wind_speed = mean(sqrt(eastward_wind^2 + northward_wind^2))) %>%
    ungroup()
}else if("relative_humidity" %in% colnames(files.train)){
  # possibly leave as relative humidity as long as others are also in relative humdity? 
  # Would need atmospheric pressure for convserion
}


# # Extract plot info & aggregate to daily resolution
# for(v in names(ncT$var)){
#   dat.hr <- matrix(ncvar_get(ncT, v), ncol=nday)
#   if(v == "air_temperature"){
#     dat.day[["air_temperature_minimum"]] <- apply(dat.hr, 2, min)
#     dat.day[["air_temperature_maximum"]] <- apply(dat.hr, 2, max)
#   } else if(v %in% c("eastward_wind", "northward_wind")) {
#     wind.e <- matrix(ncvar_get(ncT, "eastward_wind"), ncol=nday)
#     wind.n <- matrix(ncvar_get(ncT, "northward_wind"), ncol=nday)
#     wind <- sqrt(wind.e^2 + wind.n^2)
#     dat.day[["wind_speed"]] <- apply(wind, 2, mean)
#   } else {
#     dat.day[[v]] <- apply(dat.hr, 2, mean)
#   }
# }
# 

df.var <- data.frame(CF.name = c("air_temperature", "air_temperature_maximum", "air_temperature_minimum", 
                                 "surface_downwelling_longwave_flux_in_air",
                                 "air_pressure", "surface_downwelling_shortwave_flux_in_air", 
                                 "eastward_wind", "northward_wind", "wind_speed", "relative_humidity", "precipitation_flux"), 
                     units = c("Kelvin", "Kelvin", "Kelvin", "W/m2", "Pascal", "W/m2", "m/s", "m/s", "m/s", "g/g", "kg/m2/s"))

nc.info <- data.frame(CF.name = c("air_temperature_minimum", "air_temperature_maximum", "precipitation_flux", 
                                  "surface_downwelling_shortwave_flux_in_air", "surface_downwelling_longwave_flux_in_air", 
                                  "air_pressure", "specific_humidity", "wind_speed"), 
                      longname = c("2 meter minimum air temperature", "2 meter maximum air temperature", 
                                   "cumulative precipitation (water equivalent)", "incident (downwelling) showtwave radiation", 
                                   "incident (downwelling) longwave radiation", "air_pressureure at the surface", 
                                   "Relative humidity measured at 2m", 
                                   "Wind speed"), 
                      units = c("K", "K", "kg m-2 s-1", "W m-2", "W m-2", "Pa", 
                                "%", "m s-1"))


# Create a daily .nc file for each year
dim.lat <- ncdim_def(name='latitude', units='degree_north', vals=lat.nc, create_dimvar=TRUE)
dim.lon <- ncdim_def(name='longitude', units='degree_east', vals=lon.nc, create_dimvar=TRUE)
dim.time <- ncdim_def(name='time', units="sec", vals=time.day, create_dimvar=TRUE, unlim=TRUE)
nc.dim=list(dim.lat,dim.lon,dim.time)

var.list = list()
for(v in colnames(files.train)){
  var.list[[v]] = ncvar_def(name=v, units=as.character(nc.info[nc.info$CF.name==v, "units"]), dim=nc.dim, missval=-999, verbose=F)
}

loc.file <- file.path(outfolder, paste("NLDAS_day", str_pad(yr.now, width=4, side="left",  pad="0"), "nc", sep = "."))
loc <- nc_create(filename = loc.file, vars = var.list, verbose = T)

for (v in colnames(files.train)) {
  ncvar_put(nc = loc, varid = as.character(v), vals = dat.day[[v]])
}
nc_close(loc)	
# }

# -----------------------------------


