
# -----------------------------------
# Create a ncdf file from SCC observation csv file
# -----------------------------------
# Load libraries
rm(list=ls())
library(ncdf4)
library(stringr)
library(lubridate)
library(reshape2)
# Set the working directory
path.working <- "/Users/laurapuckett/Documents/Research/Fall 2018/"

# Setting some important file paths
path.pecan <- paste(path.working,"/pecan",sep = "")

# Defining a site name -- this can go into a function later
site.name = "FALLINGCREEK"
site.lat  =  37.306446
site.lon  = -79.837585 

path.scc.obs <- file.path(path.working, "/NOAA_download_downscale-master","FCR_met_14_17_20180716.csv")
file.train <- read.csv(path.scc.obs)
file.train <- cbind(file.train,colsplit(file.train$date, '/', names =  c('month','day','year')))

#
## 0. convert .csv to netcdf format
#
# path and file name, set dname
ncpath <- paste(path.working, "observations/", sep = "")
ncname <- "FCR_met_14_17_20180716_4"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "met_obs"  # note: tmp means temperature (not temporary)
#latitude, longitude, hour

longitude <- ncdim_def("longitude","degrees_northt",as.double(site.lon)) 
latitude<- ncdim_def("latitude","degrees_east",as.double(site.lat))
hour <- ncdim_def("hour","hour",as.double(file.train$time))
year <- ncdim_def("year","XXXX",as.integer(file.train$year))
month <- ncdim_def("month","XX",as.integer(file.train$month))
day <- ncdim_def("year","XXXX",as.integer(file.train$day))

# define variables
fillvalue <- 1e32 # missing data value I think?
dlname <- "ShortWave"
Shortwave.def <- ncvar_def("ShortWave"," ",list(longitude,latitude, hour),fillvalue,dlname,prec="double")
dlname <- "LongWave"
LongWave.def <- ncvar_def("LongWave","",list(longitude,latitude, hour),fillvalue,dlname,prec="double")
dlname <- "AirTemp"
AirTemp.def <- ncvar_def("AirTemp","C",list(longitude,latitude, hour),fillvalue,dlname,prec="double")
dlname <- "RelHum"
RelHum.def <- ncvar_def("RelHum","",list(longitude,latitude, hour),fillvalue,dlname,prec="double")
dlname <- "WindSpeed"
WindSpeed.def <- ncvar_def("WindSpeed","",list(longitude,latitude, hour),fillvalue,dlname,prec="double")
dlname <- "Rain"
Rain.def <- ncvar_def("Rain","",list(longitude,latitude, hour),fillvalue,dlname,prec="double")

# create netCDF file and put arrays
ncout <- nc_create(ncfname,list(Shortwave.def,LongWave.def,AirTemp.def,RelHum.def,WindSpeed.def,Rain.def),force_v4=TRUE)
# ^ this will only work if the file doesn't already exist

# put variables
ncvar_put(ncout,Shortwave.def, file.train$ShortWave)
ncvar_put(ncout,LongWave.def,file.train$LongWave)
ncvar_put(ncout,AirTemp.def,file.train$AirTemp)
ncvar_put(ncout,RelHum.def,file.train$RelHum)
ncvar_put(ncout,WindSpeed.def,file.train$WindSpeed)
ncvar_put(ncout,Rain.def,file.train$Rain)

# example of selecting variable to print
ncvar_get(ncout, "day")
