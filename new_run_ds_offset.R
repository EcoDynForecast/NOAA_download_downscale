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
# devtools::install_github("renkun-ken/formattable")
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
source("debias_and_add_error.R")
forecast.units.match = match_units(obs.data, NOAA.data)[[2]]
forecast.units.match[,"group.num"] = row(forecast.units.match)[,1]
obs.units.match = match_units(obs.data, NOAA.data)[[1]] %>%
   mutate(doy_minutes = doy,
          doy = formattable(ifelse(minute == 0, round(yday + hour/24,4),NA),4))
joined.data.original <- agg_and_join(obs.units.match, forecast.units.match) %>%
  mutate(yday = yday(timestamp))

joined.data.daily <- joined.data.original %>%
  dplyr::group_by(NOAA.member, yday) %>%
  dplyr::summarize(temp.obs = mean(temp.obs), # getting daily means from minute or 6-hourly
                   RH.obs = mean(RH.obs),
                   ws.obs = mean(ws.obs),
                   temp.for = ifelse(n() == 4, mean(temp.for), NA), # mean is NA if missing data
                   RH.for = ifelse(n() == 4, mean(RH.for), NA),
                   ws.for = ifelse(n() == 4, mean(ws.for), NA),
                   doy = formattable(first(yday),4)) %>%
  ungroup() %>%
  filter(is.na(temp.for) == FALSE && is.na(RH.for) == FALSE && is.na(ws.for) == FALSE)

joined.data <- joined.data.daily
joined.data[,"group.num"] = row(joined.data)[,1]
debiased.results <- daily_debias_and_add_error(joined.data, nmembers = 10)
debiased <- debiased.results[[1]]
debiased.with.noise <- debiased.results[[2]] %>%
  mutate(yday = as.integer(doy))

NOAA.prop <- joined.data.original %>%
  dplyr::group_by(NOAA.member, doy) %>%
  dplyr::mutate(temp.for.mean = mean(temp.for),
                RH.for.mean = mean(RH.for),
                ws.for.mean = mean(ws.for)) %>%
  ungroup() %>%
  mutate(temp.prop = temp.for/temp.for.mean, # proportion of daily mean that each 6-hourly measurement is
         RH.prop = RH.for/RH.for.mean,
         ws.prop = ws.for/ws.for.mean) %>%
  select(NOAA.member, timestamp, doy, yday,temp.for, RH.for, ws.for, temp.for.mean, RH.for.mean, ws.for.mean, temp.prop, RH.prop, ws.prop)

# redistributed = 166 days * 21 NOAA members * 4 meas/day * 10 noise members
redistributed <- inner_join(debiased.with.noise, NOAA.prop, by = c("doy","NOAA.member")) %>%
  dplyr::group_by(NOAA.member, doy) %>%
  dplyr::mutate(ds.temp = temp.mod.noise * temp.prop,
                ds.RH = RH.mod.noise * RH.prop,
                ds.ws = ws.mod.noise * ws.prop) %>%
  ungroup() %>%
  select(NOAA.member, yday, dscale.member, ds.temp, ds.RH, ds.ws)

splined.ds <- new_spline_NOAA_offset(redistributed) %>%
  mutate(doy = formattable(round(doy,4),4))
joined.obs.and.spline <- inner_join(obs.units.match, splined.ds, by = "doy")

offset <- joined.obs.and.spline %>%
  dplyr::mutate(doy.group = floor(doy)) %>%
  dplyr::group_by(NOAA.member, doy.group) %>% 
  dplyr::mutate(temp.offset = ifelse(hour == 4,interp.temp - AirTC_Avg, NA),
                temp.interp.ds = ifelse(hour >= 4, interp.temp - max(temp.offset, na.rm = TRUE), AirTC_Avg),
                ws.offset = ifelse(hour == 4, interp.ws - WS_ms_Avg, NA),
                ws.interp.ds = ifelse(hour >= 4, interp.ws - max(ws.offset, na.rm = TRUE),WS_ms_Avg),
                RH.offset = ifelse(hour == 4, interp.RH - RH, NA),
                RH.interp.ds = ifelse(hour >= 4, interp.RH - max(RH.offset, na.rm = TRUE),RH)) %>%
  ungroup()
  
  
## above is hack to select offset at 4am only, and use that value to adjust values for each group starting at 4am,
## below is an older, simpler version of this that doesn't take into account missing data - just selects 5th element of group, which is not always
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
# pdf("comparing daily_hourly_offset.pdf")
alpha = 0.3
for (i in 1:3){
  # daily mean comparisons
  my.formula <- y ~ x
  join.df <- inner_join(joined.data.daily %>% select(doy, temp.obs, RH.obs, ws.obs), debiased %>% select(-group.num), by = "doy") %>% unique()
  p1 <- ggplot(data = join.df, aes(y = get(paste(var.name[i],".obs", sep = "")), x = get(paste(var.name[i],".mod", sep = "")))) +   
    geom_point(data = joined.data.daily, aes(y = get(paste(var.name[i],".obs", sep = "")), x = get(paste(var.name[i],".for", sep = "")), col = "daily obs"), alpha = alpha) + 
    geom_point(aes(col = "debiased"), alpha = alpha) +
    geom_abline(aes(y = get(paste(var.name[i],".mod", sep = "")), x = get(paste(var.name[i],".obs", sep = ""))), slope = 1, intercept = 0, col = "red") +
    geom_smooth(method = "lm", se = FALSE, color = "black", formula = my.formula) +
    stat_poly_eq(formula = my.formula, 
                 aes(y = get(paste(var.name[i],".obs", sep = "")), x = get(paste(var.name[i],".mod", sep = "")), label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE) +
    ylab(" daily mean obs") + 
    xlab("daily mean NOAA (original or debiased)") +
    scale_colour_brewer(palette = "Set1") +
    ggtitle(paste("daily mean:", vars.title.list[i])) + 
    theme(legend.position="bottom")
  
  # residuals from daily comparison
  p2 <- ggplot() +
    geom_point(data = joined.data.daily, aes(x = get(paste(var.name[i],".obs", sep = "")), y = get(paste(var.name[i],".for", sep = "")) - get(paste(var.name[i],".obs", sep = "")), color = "daily obs"), alpha = alpha) +
    geom_point(data = join.df, aes(x = get(paste(var.name[i],".obs", sep = "")), y = get(paste(var.name[i],".mod", sep = ""))- get(paste(var.name[i],".obs", sep = "")), color = "debiased"), alpha = alpha) + 
    scale_colour_brewer(palette = "Set1") +
    ggtitle(paste("residuals vs obs:", vars.title.list[i])) + 
    theme(legend.position="bottom") + 
    xlab("daily mean obs") + 
    ylab("redisuals (forecast - obs)")
  
  # # offset method
  # p3 <- ggplot(data = offset, aes(y = get(var.name.obs[i]), x = get(paste("interp.",var.name[i], sep = "")))) +
  #   geom_point(aes(color = "hourly spline"), alpha = alpha) + 
  #   geom_point(data = offset, aes(y = get(var.name.obs[i]), x = get(paste(var.name[i],".interp.ds", sep = "")), color = "hourly offset + spline + spatial downscaling"), alpha = alpha) + 
  #   geom_smooth(method = "lm", se = FALSE, color = "black", formula = my.formula) +
  #   stat_poly_eq(formula = my.formula, 
  #                aes(y = get(var.name.obs[i]), x = get(paste(var.name[i],".interp.ds", sep = "")), label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
  #                parse = TRUE) +
  #   ylab("original obs") + 
  #   xlab("splined or spline + offset") +
  #   scale_colour_brewer(palette = "Set1") +
  #   ggtitle(paste("spline and offset:", vars.title.list[i])) + 
  #   theme(legend.position="bottom")
  # 
  # p4 <- ggplot() +
  #   geom_point(data = offset, aes(y = get(var.name.obs[i]) - get(paste("interp.",var.name[i], sep = "")), x = get(paste("interp.",var.name[i], sep = "")), color = "residuals: hourly spline"), alpha = alpha) +
  #   geom_point(data = offset, aes(y = get(var.name.obs[i]) - get(paste(var.name[i],".interp.ds", sep = "")), x = get(paste(var.name[i],".interp.ds", sep = "")), color = "residuals: offset "), alpha = alpha) + 
  #   scale_colour_brewer(palette = "Set1") +
  #   ggtitle(paste("residuals vs obs:", vars.title.list[i])) + 
  #   theme(legend.position="bottom") + 
  #   xlab("daily mean obs") + 
  #   ylab("redisuals (forecast - obs)")
  start_day = 280
  end_day = 284
  p3 <- ggplot(data = offset %>% filter(doy <=end_day & doy >= start_day)) +
    geom_line(aes(x = doy, y = AirTC_Avg, color = "observations")) + 
    geom_line(aes(x = doy, y = temp.interp.ds, color = "downscaled", group = interaction(NOAA.member, dscale.member)))
  print(p3)
  
  png(paste("./daily.downscale.spline.offset.1.2.3.4.",var.name[i], ".png", sep = ""), width = 1024, height = 768)
  grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
  dev.off()
  # readPNG(source = paste(paste("./p1.2.3.4.",var.name[i], ".png", sep = "")))
}
# dev.off()


  start_day = 220
  end_day = 223
  ggplot() +
    geom_line(data = offset %>% filter(doy <=end_day & doy >= start_day), aes(x = doy, y = AirTC_Avg, color = "observations"), alpha = alpha) + 
    geom_line(data = offset %>% filter(doy <=end_day & doy >= start_day), aes(x = doy, y = interp.temp, color = "splined NOAA", group = NOAA.member), alpha = alpha) + 
    geom_line(data = offset %>% filter(doy <= end_day & doy >= start_day), aes(x = doy, y = temp.interp.ds, color = "ds + splined", group = NOAA.member), alpha = alpha)


# scatter.original(joined.data, var.name[i], plot.title = paste("obs vs NOAA:", vars.title.list[i]))


