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

rm(list = ls())
library(lubridate)
library(dplyr)
library(plyr)
library(tidyr)
library(ggpmisc)
library(gridExtra)
library(grid)
library(png)
path.working <- "/Users/laurapuckett/Documents/Research/Fall 2018/"
setwd(path.working)
path.my.files <- paste(path.working, "/my_files/",sep = "")
NOAA.flux <- readRDS(paste(path.working, "my_files/","NOAA.flux.forecasts.10.17", sep = ""))
NOAA.state <- readRDS(paste(path.working, "my_files/","NOAA.state.forecasts.10.17", sep = ""))
NOAA.data = inner_join(NOAA.flux, NOAA.state, by = c("forecast.date.hour","ensembles"))
obs.data <- read.csv(paste(path.working, "my_files/", "FCRmet.10.17.csv", sep = ""),header = TRUE)

setwd(path.my.files)
source("match_units.R")
source("agg_and_join.R")
source("debias_and_add_error.R")
source("spline_NOAA.R")
source("plot_spline.R")
source("new.plot_spline.R")
source("summary_plottting.R")
forecast.units.match = match_units(obs.data, NOAA.data)[[2]]
obs.units.match = match_units(obs.data, NOAA.data)[[1]]
forecast.units.match[,"group.num"] = row(forecast.units.match)[,1]
joined.data <- agg_and_join(obs.units.match, forecast.units.match)
joined.data[,"group.num"] = row(joined.data)[,1]
debiased.results <- debias_and_add_error(joined.data, nmembers = 5)
debiased <- debiased.results[[1]]
debiased.with.noise <- debiased.results[[2]]

debiased.dowscaled.results <- spline_NOAA(debiased, debiased.with.noise)
debiased.downscaled <- debiased.dowscaled.results[[1]]
debiased.downscaled.with.noise <- debiased.dowscaled.results[[2]]


expanded.forecast.units.match <- forecast.units.match %>%
  group_by(NOAA.member, group.num, doy, temp, ws, RH) %>%
  tidyr::expand(doy = c(doy - 6/24,doy - 5/24,doy - 4/24,doy - 3/24,doy - 2/24,doy - 1/24, doy)) %>%
  ungroup()

expanded.debiased.with.noise <- debiased.with.noise  %>%
  group_by(NOAA.member, group.num, dscale.member, doy, temp.mod.noise, ws.mod.noise, RH.mod.noise) %>%
  tidyr::expand(doy = c(doy - 6/24,doy - 5/24,doy - 4/24,doy - 3/24,doy - 2/24,doy - 1/24, doy)) %>%
  ungroup()

joined.obs.and.ds <- inner_join(obs.units.match, debiased.downscaled, by = "doy")
joined.obs.and.NOAA <- inner_join(obs.units.match, forecast.units.match, by = "doy")

pdf("./summary_plots_grid_10_26.pdf")
var.name =  c("temp","RH","ws")
var.name.obs = c("AirTC_Avg","RH","WS_ms_Avg")
vars.title.list = c("Temperature [C]","Relative Humidity [%]","Average Wind Speed [m/s]")
for (i in 1:length(var.name)){
  alpha = 0.5
  plot.1 <- scatter.original(joined.data, var.name[i], plot.title = paste("obs vs NOAA:", vars.title.list[i]))
  #print(plot.1)
  plot.blank <-  ggplot()
  
 # residuals_vs_doy(joined.data, var.name[i], plot.title = paste("obs vs NOAA:", vars.title.list[i]))
  # obs_vs_NOAA_fitting(joined.data, var.name[i], plot.title = paste("obs vs NOAA:", vars.title.list[i]))
  plot.2 <- scatter.debiased(obs.units.match, debiased, debiased.with.noise, var.name[i], var.name.obs[i], plot.title = paste("obs vs debiased:", vars.title.list[i]))
 # print(plot.2)
  # 
   plot.3 <- scatter.debiased.downscaled(obs.units.match, debiased.downscaled, debiased.downscaled.with.noise, var.name[i], var.name.obs[i], plot.title = paste("obs vs debiased & downscaled:", vars.title.list[i]))
  # 
   plot.4 <- new_plot_spline(expanded.forecast.units.match, obs.units.match, expanded.debiased.with.noise, debiased.downscaled, joined.obs.and.ds, joined.obs.and.NOAA, var.name[i], var.name.obs[i], plot.title = paste("Comparison over time", vars.title.list[i]), 115,5)
  # 
   plot.5 <- new_plot_spline(expanded.forecast.units.match, obs.units.match, expanded.debiased.with.noise, debiased.downscaled, joined.obs.and.ds, joined.obs.and.NOAA, var.name[i], var.name.obs[i],  plot.title = paste("Comparison over time", vars.title.list[i]), 200, 5)
  # png("/Users/laurapuckett/Documents/Research/Fall 2018/my_files/new.png")
   png(paste("./plot.page.",var.name[i], ".png", sep = ""), width = 2048, height = 1536)
   grid.arrange(plot.1, plot.blank, plot.2, plot.3, plot.4, plot.5, ncol = 2, nrow = 3)
   # print(plot.page)
   dev.off()
  
   readPNG(source = paste("./plot.page.",var.name[i], ".png", sep = ""))
}
 dev.off()

# source(plot.hist.and.scatter.R)
# source(plot.hist.difference.and.scatter)
# source(check.daily.seasonal.cycles.R)
# plot.hist.and.scatter(joined.data,vars.list, vars.title.list, paste(path.output, "/Hist_forecast_obs.pdf", sep = ""))
# plot.hist.difference.and.scatter(joined.6.hourly,vars.list, vars.title.list, paste(path.output, "/Hist_forecast_minus_obs.pdf", sep = ""))
# check.daily.seasonal.cycles(joined.6.hourly,vars.list, vars.title.list, paste(path.output, "/Timeseries.scatter.pdf", sep = ""))
# vars.list = c("temp","RH","avg.sw","avg.lw","precip.rate","ws")
# vars.title.list = c("Temperature [C]","Relative Humidity [%]","Shortwave Radiation [W/m2]","Longwave Radiation [W/m2]","Precipitation Rate [kg/m2/s]","Average Wind Speed [m/s]")
