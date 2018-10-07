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
setwd("/Users/laurapuckett/Documents/Research/Fall 2018/")
path.output <- paste(path.working, "/my_files/",sep = "")
NOAA.data <- readRDS(paste(path.working, "my_files/","NOAA.first.day.Rdata", sep = ""))
obs.data <- read.csv("/Users/laurapuckett/Documents/Research/Fall 2018/observations/carina-scc-obs/FCRmet.csv", header = TRUE)

match_units(obs.data, NOAA.data)
obs.units.match = readRDS(file = paste(path.working,"/obs.units.match.RData",sep= ""))
forecast.units.match = readRDS(file = paste(path.working,"/forecast.units.match.RData",sep= ""))
joined.data <- agg_and_join(obs.units.match, forecast.units.match)
debiased <- debias_and_add_error(joined.data, nmembers = 20)
debiased.downscaled <- spline_NOAA(debiased)

forecast.units.match[,"group.num"] = row(forecast.units.match)[,1] # create a group number for each original NOAA forecast entry
expanded.forecast.units.match <- forecast.units.match%>%
  group_by(NOAA.member, group.num, doy, temp, ws, RH) %>%
  tidyr::expand(doy = c(doy - 6/24,doy - 5/24,doy - 4/24,doy - 3/24,doy - 2/24,doy - 1/24, doy)) %>%
  ungroup()

debiased[,"group.num"] = row(debiased)[,1] # create a group number for each original NOAA forecast entry
expanded.debiased <- debiased %>%
  group_by(NOAA.member, group.num, doy, temp.mod, ws.mod, RH.mod) %>%
  tidyr::expand(doy = c(doy - 6/24,doy - 5/24,doy - 4/24,doy - 3/24,doy - 2/24,doy - 1/24, doy)) %>%
  ungroup()

plot_spline(expanded.forecast.units.match, obs.units.match, expanded.debiased, debiased.downscaled, 115,5)


# source(plot.hist.and.scatter.R)
# source(plot.hist.difference.and.scatter)
# source(check.daily.seasonal.cycles.R)
# plot.hist.and.scatter(joined.data,vars.list, vars.title.list, paste(path.output, "/Hist_forecast_obs.pdf", sep = ""))
# plot.hist.difference.and.scatter(joined.6.hourly,vars.list, vars.title.list, paste(path.output, "/Hist_forecast_minus_obs.pdf", sep = ""))
# check.daily.seasonal.cycles(joined.6.hourly,vars.list, vars.title.list, paste(path.output, "/Timeseries.scatter.pdf", sep = ""))
# vars.list = c("temp","RH","avg.sw","avg.lw","precip.rate","ws")
# vars.title.list = c("Temperature [C]","Relative Humidity [%]","Shortwave Radiation [W/m2]","Longwave Radiation [W/m2]","Precipitation Rate [kg/m2/s]","Average Wind Speed [m/s]")
