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
devtools::install_github("renkun-ken/formattable")
library(formattable)
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
# source("debias_and_add_error.R")
source("spline_NOAA_offset.R")
source("plot_spline.R")
source("new.plot_spline.R")
source("summary_plottting.R")
forecast.units.match = match_units(obs.data, NOAA.data)[[2]]
obs.units.match = match_units(obs.data, NOAA.data)[[1]] %>%
   mutate(doy_minutes = doy,
          doy = formattable(ifelse(minute == 0, round(yday + hour/24,4),NA),4))
forecast.units.match[,"group.num"] = row(forecast.units.match)[,1]
joined.data <- agg_and_join(obs.units.match, forecast.units.match)
joined.data[,"group.num"] = row(joined.data)[,1]
splined.NOAA <- spline_NOAA_offset(joined.data) %>%
  mutate(doy = formattable(round(doy,4),4))
joined.obs.and.spline <- inner_join(obs.units.match, splined.NOAA, by = "doy")
offset <- joined.obs.and.spline %>%
  dplyr::mutate(doy.group = floor(doy)) %>%
  dplyr::group_by(NOAA.member, doy.group) %>% 
  dplyr::mutate(temp.offset = ifelse(hour == 4,interp.temp - AirTC_Avg, NA),
                temp.interp.ds = ifelse(hour >= 4, interp.temp - max(temp.offset, na.rm = TRUE), AirTC_Avg),
                ws.offset = ifelse(hour == 4, interp.ws - WS_ms_Avg, NA),
                ws.interp.ds = ifelse(hour >= 4, interp.ws - max(ws.offset, na.rm = TRUE),WS_ms_Avg),
                RH.offset = ifelse(hour == 4, interp.RH - RH, NA),
                RH.interp.ds = ifelse(hour >= 4, interp.RH - max(RH.offset, na.rm = TRUE),RH))
  
## above is hack to select offset at 4am only, and use that value to adjust values for each group starting at 4am,
## below is an older, simpler version of this that doesn't take into account missing data - just selects 5th element of group, which is not always 4am
## 

  # dplyr::mutate(temp.offset = interp.temp[5] - AirTC_Avg[5],
  #               ws.offset = interp.ws[5] - WS_ms_Avg[5],
  #               RH.offset = interp.RH[5] - RH[5],
  #               temp.interp.ds = ifelse(hour > 3, interp.temp - temp.offset, AirTC_Avg),
  #               ws.interp.ds = ifelse(hour > 3, interp.ws - ws.offset, WS_ms_Avg),
  #               RH.interp.ds = ifelse(hour > 3, interp.RH - RH.offset, RH))


var.name =  c("temp","RH","ws")
var.name.obs = c("AirTC_Avg","RH","WS_ms_Avg")
vars.title.list = c("Temperature [C]","Relative Humidity [%]","Average Wind Speed [m/s]")
for (i in 1:3){
  alpha = 0.3
  plot.1 <- scatter.original(joined.data, var.name[i], plot.title = paste("obs vs NOAA:", vars.title.list[i]))
  start_day = 220
  end_day = 223
 ggplot() +
    geom_line(data = offset %>% filter(doy <=end_day & doy >= start_day), aes(x = doy, y = AirTC_Avg, color = "observations"), alpha = alpha) + 
    geom_line(data = offset %>% filter(doy <=end_day & doy >= start_day), aes(x = doy, y = interp.temp, color = "splined NOAA", group = NOAA.member), alpha = alpha) + 
    geom_line(data = offset %>% filter(doy <= end_day & doy >= start_day), aes(x = doy, y = temp.interp.ds, color = "ds + splined", group = NOAA.member), alpha = alpha)
}

  

