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
library(plyr)
library(tidyr)
library(ggpmisc)
library(gridExtra)
path.working <- "/Users/laurapuckett/Documents/Research/Fall 2018/"
path.output <- paste(path.working, "/my_files/",sep = "")
NOAA.data <- readRDS(paste(path.working, "my_files/","NOAA.first.day.Rdata", sep = ""))
obs.data <- read.csv("/Users/laurapuckett/Documents/Research/Fall 2018/observations/carina-scc-obs/FCRmet.csv", header = TRUE)

vars.list = c("temp","RH","avg.sw","avg.lw","precip.rate","ws")
vars.title.list = c("Temperature [C]","Relative Humidity [%]","Shortwave Radiation [W/m2]","Longwave Radiation [W/m2]","Precipitation Rate [kg/m2/s]","Average Wind Speed [m/s]")


source(join.obs.and.NOAA)
joined.data <- join.obs.and.NOAA(obs.data, NOAA.data)

source(plot.hist.and.scatter.R)
source(plot.hist.difference.and.scatter)
source(check.daily.seasonal.cycles.R)
plot.hist.and.scatter(joined.data,vars.list, vars.title.list, paste(path.output, "/Hist_forecast_obs.pdf", sep = ""))
plot.hist.difference.and.scatter(joined.6.hourly,vars.list, vars.title.list, paste(path.output, "/Hist_forecast_minus_obs.pdf", sep = ""))
check.daily.seasonal.cycles(joined.6.hourly,vars.list, vars.title.list, paste(path.output, "/Timeseries.scatter.pdf", sep = ""))

source(debias_and_add_error.R)
output = debias_and_add_error(joined.data, nmembers = 20)
