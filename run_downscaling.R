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
# more diagnostics - make a page within a pdf for each variable
# predicted vs observed, 1:1 vs fit line, example of time series, hist of residuals, dist of residuals over time, is mod-obs residual a function of temperature? 

# --------------------------------------
# 0. Setup
# --------------------------------------

# rm(list = ls())
library(lubridate)
library(dplyr)
library(plyr)
library(tidyr)
library(ggpmisc)
library(gridExtra)
path.working <- "/Users/laurapuckett/Documents/Research/Fall 2018/"
setwd(path.working)
path.my.files <- paste(path.working, "/my_files/",sep = "")
NOAA.data <- readRDS(paste(path.working, "my_files/","NOAA.first.day.Rdata", sep = ""))
obs.data <- read.csv("/Users/laurapuckett/Documents/Research/Fall 2018/observations/carina-scc-obs/FCRmet.csv", header = TRUE)

setwd(path.my.files)
source("match_units.R")
source("agg_and_join.R")
source("debias_and_add_error.R")
source("spline_NOAA.R")
source("plot_spline.R")

match_units(obs.data, NOAA.data)
obs.units.match = readRDS(file = paste(path.working,"/obs.units.match.RData",sep= ""))
forecast.units.match = readRDS(file = paste(path.working,"/forecast.units.match.RData",sep= ""))
forecast.units.match[,"group.num"] = row(forecast.units.match)[,1]
joined.data <- agg_and_join(obs.units.match, forecast.units.match)
joined.data[,"group.num"] = row(joined.data)[,1]
debiased.with.noise <- debias_and_add_error(joined.data, nmembers = 5)

expanded.debiased.with.noise <- debiased.with.noise  %>%
  group_by(NOAA.member, group.num, dscale.member, doy, temp.mod.noise, ws.mod.noise, RH.mod.noise) %>%
  tidyr::expand(doy = c(doy - 6/24,doy - 5/24,doy - 4/24,doy - 3/24,doy - 2/24,doy - 1/24, doy)) %>%
  ungroup()

debiased.downscaled <- spline_NOAA(debiased.with.noise)


expanded.forecast.units.match <- forecast.units.match %>%
  group_by(NOAA.member, group.num, doy, temp, ws, RH) %>%
  tidyr::expand(doy = c(doy - 6/24,doy - 5/24,doy - 4/24,doy - 3/24,doy - 2/24,doy - 1/24, doy)) %>%
  ungroup()

joined.obs.and.ds <- inner_join(obs.units.match, debiased.downscaled, by = "doy")
joined.obs.and.NOAA <- inner_join(obs.units.match, forecast.units.match, by = "doy")

plot_spline(expanded.forecast.units.match, obs.units.match, expanded.debiased.with.noise, debiased.downscaled, joined.obs.and.ds, joined.obs.and.NOAA, 145,5)


# source(plot.hist.and.scatter.R)
# source(plot.hist.difference.and.scatter)
# source(check.daily.seasonal.cycles.R)
# plot.hist.and.scatter(joined.data,vars.list, vars.title.list, paste(path.output, "/Hist_forecast_obs.pdf", sep = ""))
# plot.hist.difference.and.scatter(joined.6.hourly,vars.list, vars.title.list, paste(path.output, "/Hist_forecast_minus_obs.pdf", sep = ""))
# check.daily.seasonal.cycles(joined.6.hourly,vars.list, vars.title.list, paste(path.output, "/Timeseries.scatter.pdf", sep = ""))
# vars.list = c("temp","RH","avg.sw","avg.lw","precip.rate","ws")
# vars.title.list = c("Temperature [C]","Relative Humidity [%]","Shortwave Radiation [W/m2]","Longwave Radiation [W/m2]","Precipitation Rate [kg/m2/s]","Average Wind Speed [m/s]")
