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
forecast <- read.csv("/Users/laurapuckett/Documents/Research/Fall 2018/my_files/SCCData-noaa-data/20180613gep_all_00z.csv") %>%
  dplyr::mutate(forecast.date = as_datetime(forecast.date))
forecast.expanded <- forecast
forecast.expanded[,"group.num"] = row(forecast.expanded)[,1]
forecast.expanded <- forecast.expanded %>% group_by(group.num, ensembles, tmp2m) %>%
  tidyr::expand(forecast.date = c(forecast.date - 6*3600,forecast.date - 5*3600,forecast.date - 4*3600,forecast.date - 3*3600,forecast.date - 2*3600,forecast.date - 3600, forecast.date)) %>%
  ungroup() %>%
  arrange(group.num, forecast.date) %>%
  group_by(group.num) %>%
  dplyr::mutate(lower = min(tmp2m),
                upper = max(tmp2m)) %>% 
  ungroup()

forecast <- forecast %>%
  group_by(forecast.date) %>%
  dplyr::mutate(lower = min(tmp2m),
                upper = max(tmp2m)) %>%
  ungroup()

ggplot(data = forecast.expanded, aes(x = yday(forecast.date) + hour(forecast.date)/24, y = tmp2m - 273.15)) +
  geom_line(aes(group = ensembles)) +
  ggtitle("16-day NOAA GEFS forecast") + 
  xlab("day of year") +
  ylab("Temperature [C]") +
  # scale_x_continuous(minor_breaks = seq(163 , 180, 0.25), breaks = seq(163, 180, 1), limits = c(15,35)) + 
  scale_y_continuous(minor_breaks = seq(15 , 35, 2.5), breaks = seq(15 , 35, 5)) +
  # geom_ribbon(aes(ymin = lower, ymax = upper, group = group.num), fill = "grey70", alpha = 0.5) + 
  theme(text = element_text(size=25))

ggplot(data = forecast, aes(x = yday(forecast.date) + hour(forecast.date)/24, y = tmp2m - 273.15)) +
  geom_line(aes(group = ensembles)) +
  ggtitle("16-day NOAA GEFS forecast") + 
  xlab("day of year") +
  ylab("Temperature [C]") +
  # scale_x_continuous(minor_breaks = seq(163 , 180, 0.25), breaks = seq(163, 180, 1), limits = c(15,35)) + 
  scale_y_continuous(minor_breaks = seq(15 , 35, 2.5), breaks = seq(15 , 35, 5)) +
  geom_ribbon(aes(x = yday(forecast.date) + hour(forecast.date)/24, ymin = lower-273.15, ymax = upper-273.15), fill = "seagreen3", alpha = 0.8) +
  theme(text = element_text(size=25))


path.working <- "/Users/laurapuckett/Documents/Research/Fall 2018/"
obs <- read.csv(paste(path.working, "my_files/", "FCRmet.csv", sep = ""),header = TRUE)

setwd(path.working)
path.my.files <- paste(path.working, "/my_files/",sep = "")
setwd(path.my.files)
source("match_units.R")
source("agg_and_join.R")
source("spline_NOAA_offset.R")
source("new_spline_NOAA_offset.R")
source("new.plot_spline.R")
source("summary_plottting.R")
source("debias_and_add_error.R")
source("daily_debias_and_add_error.R")

forecast.units.match = match_units(obs.data, forecast)[[2]]
forecast.units.match[,"group.num"] = row(forecast.units.match)[,1]
obs.units.match = match_units(obs.data, NOAA.data)[[1]] %>%
  dplyr::mutate(doy_minutes = doy,
                doy = formattable(ifelse(minute == 0, round(yday + hour/24,4),NA),4))
joined.data.original <- agg_and_join(obs.units.match, forecast.units.match) %>%
  mutate(yday = yday(timestamp))  %>%
  dplyr::group_by(NOAA.member, yday)  %>%
  dplyr::mutate(n = n(),
                temp.for = ifelse(n == 4, temp.for, NA), # force NA for days without 4 NOAA entries
                RH.for = ifelse(n == 4, RH.for, NA),
                ws.for = ifelse(n == 4, ws.for, NA)) %>%
  ungroup()

ggplot() + 
  geom_line()
