assess_downscaling <- function(start_time, end_time){
# would need to use this over the past 2 weeks to compare against obs
# reruns at each function call to get up to date ds_output and ds_ouput_no_noise files
file_name = paste(year(start_time), 
                  ifelse(month(start_time)<10,
                                           paste("0",month(start_time),sep = ""),
                                           month(start_time)),
                  ifelse(day(start_time)<10,
                      paste("0",day(start_time),sep = ""),
                      day(start_time)),
                  "gep_all_00z", sep = "")
process_GEFS2GLM_v2(file_name, DOWNSCALE_MET = TRUE, ADD_NOISE = TRUE)
process_GEFS2GLM_v2(file_name, DOWNSCALE_MET = TRUE, ADD_NOISE = FALSE)
# process_GEFS2GLM_v2(file_name, DOWNSCALE_MET = FALSE, ADD_NOISE = FALSE) # not currently using this is analysis

load(file = '/Users/laurapuckett/Documents/Research/Fall 2018/my_files/ds_output.RData')
load(file = '/Users/laurapuckett/Documents/Research/Fall 2018/my_files/ds_output_no_noise.RData')
ds_output <- ds_output %>% plyr::rename(c("full_time" = "timestamp")) %>%
  mutate(timestamp = as_datetime(paste(timestamp, ':00', sep = "")))
ds_output_no_noise <- ds_output_no_noise %>% plyr::rename(c("full_time" = "timestamp")) %>%
  mutate(timestamp = as_datetime(paste(timestamp, ':00', sep = "")))

full_time.df = as.data.frame(full_time)
# LongWave.df = as.data.frame(LongWave) %>% cbind(full_time.df) %>%
#   gather(NOAA.member, LongWave, V1:V21)
# Rain.df = as.data.frame(Rain)  %>% cbind(full_time.df) %>%
#   gather(NOAA.member, Rain, V1:V21)
# Snow.df = as.data.frame(Snow)  %>% cbind(full_time.df) %>%
#   gather(NOAA.member, Snow, V1:V21)
# AirTemp.df = as.data.frame(AirTemp) %>% cbind(full_time.df) %>%
#   gather(NOAA.member, AirTemp, V1:V21)
# WindSpeed.df = as.data.frame(WindSpeed)  %>% cbind(full_time.df) %>%
#   gather(NOAA.member, WindSpeed, V1:V21)
# RelHum.df = as.data.frame(RelHum)  %>% cbind(full_time.df) %>%
#   gather(NOAA.member, RelHum, V1:V21)
# ShortWave.df = as.data.frame(ShortWave) %>% cbind(full_time.df) %>%
#   gather(NOAA.member, ShortWave, V1:V21)
library(stringr)
library(dplyr)
# GLM_climate = LongWave.df %>%
#   inner_join(Rain.df, by = c("full_time","NOAA.member")) %>%
#   inner_join(Snow.df, by = c("full_time","NOAA.member")) %>%
#   inner_join(AirTemp.df, by = c("full_time","NOAA.member")) %>%
#   inner_join(WindSpeed.df, by = c("full_time","NOAA.member")) %>%
#   inner_join(RelHum.df, by = c("full_time","NOAA.member")) %>%
#   inner_join(ShortWave.df, by = c("full_time","NOAA.member")) %>%
#   dplyr::mutate(NOAA.member = as.integer(str_replace(NOAA.member, "V",""))) %>%
#   inner_join(ds_output, by = c("full_time","NOAA.member"), suffix = c('.no.ds', '.ds')) %>%
#   plyr::rename(c("full_time" = "timestamp")) %>%
#     mutate(timestamp = as_datetime(paste(timestamp)))

obs.data <- read.csv(paste('/Users/laurapuckett/Documents/Research/Fall 2018/', "my_files/", "FCRmet.csv", sep = ""), header = TRUE)
source("/Users/laurapuckett/Documents/Research/Fall 2018/my_files/prep_obs.R")
obs.units.match = prep_obs(obs.data)
obs.units.match <- obs.units.match %>%
  # max air temp record in Vinton, VA is 40.6 C 
  # coldest air temp on record in Vinton, Va is -23.9 C
  # http://www.climatespy.com/climate/summary/united-states/virginia/roanoke-regional 
  # lots of bad data for longwave between 8/23/2018 and 9/11/2018 randomly for a couple minutes at a       # time. Removing entire section of data for now. Also bad data on a few other days
  dplyr::mutate(AirTK_Avg = ifelse(AirTK_Avg > 273.15 + 41, NA, AirTK_Avg),
                AirTK_Avg = ifelse(AirTK_Avg < 273.15 -23.9, NA, AirTK_Avg),
                SR01Up_Avg = ifelse(SR01Up_Avg < 0, 0, SR01Up_Avg),
                IR01DnCo_Avg = ifelse(IR01DnCo_Avg <0, NA, IR01DnCo_Avg),
                IR01DnCo_Avg = ifelse(month(timestamp) > 6 & month(timestamp) < 10 & IR01DnCo_Avg < 410,NA,IR01DnCo_Avg)) %>%
   filter(as_datetime(timestamp) > as_datetime(start_time)) %>%
  filter(as_datetime(timestamp) < as_datetime(end_time)) %>%
   dplyr::mutate(hour = hour(timestamp),
                 date = date(timestamp)) %>%
  dplyr::mutate(timestamp = as_datetime(paste(date, " ", hour, ":","00:00", sep = ""), tz = "UTC"))
#   dplyr::group_by(date, hour) %>%
#   dplyr::summarize(avg.temp.hrly = mean(AirTK_Avg-273.15, na.rm = TRUE),
#                    avg.sw.hrly = mean(SR01Up_Avg),
#                    avg.lw.hrly = mean(IR01DnCo_Avg),
#                    avg.ws.hrly = mean(WS_ms_Avg),
#                    avg.RH.hrly = mean(RH)) %>%
#   dplyr::mutate(timestamp = as_datetime(paste(date, " ", hour, ":","00:00", sep = ""), tz = "US/Eastern"))

forecast.summary.table = data_frame(metric = c("temp","RH","ws","sw","lw"),  r2.no.noise.ds = rep(NA,5), r2.ds.obs = rep(NA,5),mean.residual.no.noise = rep(NA,5), mean.residual.with.noise = rep(NA,5))

joined.no.noise <- dplyr::inner_join(obs.units.match, ds_output_no_noise, by = "timestamp") %>% group_by(timestamp, NOAA.member) %>%
  summarize(AirTemp = first(AirTemp),
            WindSpeed = first(WindSpeed),
            RelHum = first(RelHum),
            ShortWave = first(ShortWave),
            AirTK_Avg = first(AirTK_Avg),
            # LongWave = first(LongWave), # come back to longwave later
            RH = first(RH),
            SR01Up_Avg = first(SR01Up_Avg),
            WS_ms_Avg = first(WS_ms_Avg),
            IR01DnCo_Avg = first(IR01DnCo_Avg)) %>%
  ungroup() %>%
  mutate(date = date(timestamp)) %>%
  group_by(date) %>%
  dplyr::mutate(daily_IR01DnCo_Avg = mean(IR01DnCo_Avg)) %>%
  ungroup()

joined.with.noise <- dplyr::inner_join(obs.units.match, ds_output, by = "timestamp") %>% group_by(timestamp, NOAA.member, dscale.member) %>%
  summarize(AirTemp = first(AirTemp),
            WindSpeed = first(WindSpeed),
            RelHum = first(RelHum),
            ShortWave = first(ShortWave),
            AirTK_Avg = first(AirTK_Avg),
            # LongWave = first(LongWave), # come back to longwave later
            RH = first(RH),
            SR01Up_Avg = first(SR01Up_Avg),
            WS_ms_Avg = first(WS_ms_Avg),
            IR01DnCo_Avg = first(IR01DnCo_Avg)) %>%
  ungroup() %>%
  mutate(date = date(timestamp)) %>%
  group_by(date) %>%
  dplyr::mutate(daily_IR01DnCo_Avg = mean(IR01DnCo_Avg)) %>%
  ungroup()

formula = lm(joined.no.noise$AirTK_Avg - 273.15 ~ joined.no.noise$AirTemp)
forecast.summary.table[1,2] = summary(lm(formula))$r.squared
forecast.summary.table[1,4] = mean(lm(formula)$residuals)

formula = lm(joined.no.noise$RH ~ joined.no.noise$RelHum)
forecast.summary.table[2,2] = summary(lm(formula))$r.squared
forecast.summary.table[2,4] = mean(lm(formula)$residuals)

formula = lm(joined.no.noise$WS_ms_Avg  ~ joined.no.noise$WindSpeed)
forecast.summary.table[3,2] = summary(lm(formula))$r.squared
forecast.summary.table[3,4] = mean(lm(formula)$residuals)

formula = lm(joined.no.noise$SR01Up_Avg ~ joined.no.noise$ShortWave)
forecast.summary.table[4,2] = summary(lm(formula))$r.squared
forecast.summary.table[4,4] = mean(lm(formula)$residuals)

# formula = lm(joined.no.noise$daily_IR01DnCo_Avg ~ joined.no.noise$LongWave)
# forecast.summary.table[5,2] = summary(lm(formula))$r.squared
# forecast.summary.table[5,4] = mean(lm(formula)$residuals)

formula = lm(joined.with.noise$AirTK_Avg - 273.15 ~ joined.with.noise$AirTemp)
forecast.summary.table[1,3] = summary(lm(formula))$r.squared
forecast.summary.table[1,5] = mean(lm(formula)$residuals)

formula = lm(joined.with.noise$RH ~ joined.with.noise$RelHum)
forecast.summary.table[2,3] = summary(lm(formula))$r.squared
forecast.summary.table[2,5] = mean(lm(formula)$residuals)

formula = lm(joined.with.noise$WS_ms_Avg  ~ joined.with.noise$WindSpeed)
forecast.summary.table[3,3] = summary(lm(formula))$r.squared
forecast.summary.table[3,5] = mean(lm(formula)$residuals)

formula = lm(joined.with.noise$SR01Up_Avg ~ joined.with.noise$ShortWave)
forecast.summary.table[4,3] = summary(lm(formula))$r.squared
forecast.summary.table[4,5] = mean(lm(formula)$residuals)
forecast.summary.table

# formula = lm(joined.no.noise$daily_IR01DnCo_Avg ~ joined.with.noise$LongWave)
# forecast.summary.table[5,3] = summary(lm(formula))$r.squared
# forecast.summary.table[5,5] = mean(lm(formula)$residuals)
print(forecast.summary.table)
return(forecast.summary.table)
}
# forecast.summary.table
# ggplot(data = joined, aes(x = timestamp), alpha = 0.5)+
#   geom_point(aes(y = AirTemp.ds, color = "ds")) + 
#   geom_point(aes(y = AirTemp.no.ds, color = "ns ds")) +
#   geom_point(aes(y = avg.temp.hrly, color = "obs"))
# ggplot(data = joined, aes(x = AirTemp, y = avg.temp.hrly)) +
#   geom_point()
# ggplot(data = joined, aes(x = timestamp)) +
#   geom_line(aes(y = AirTK_Avg - 273.15, color = "obs")) +
#   geom_point(aes(y = AirTemp, color = "ds no noise"), alpha = 0.3)
# # also need to check confidence intervals
