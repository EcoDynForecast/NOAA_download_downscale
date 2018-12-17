
# -----------------------------------
# 1.0 prepare houlry observations dataset 
# -----------------------------------
source("prep_obs.R")
source("check_CI.R")
obs.data <- read.csv(paste(path.working, "FCRmet.csv", sep = ""), header = TRUE)
obs.units.match = prep_obs(obs.data) %>%
  # max air temp record in Vinton, VA is 40.6 C 
  # coldest air temp on record in Vinton, Va is -23.9 C
  # http://www.climatespy.com/climate/summary/united-states/virginia/roanoke-regional 
  # lots of bad data for longwave between 8/23/2018 and 9/11/2018 randomly for a couple minutes at a       # time. Removing entire section of data for now. Also bad data on a few other days
  dplyr::mutate(AirTK_Avg = ifelse(AirTK_Avg > 273.15 + 41, NA, AirTK_Avg),
                AirTK_Avg = ifelse(AirTK_Avg < 273.15 -23.9, NA, AirTK_Avg),
                SR01Up_Avg = ifelse(SR01Up_Avg < 0, 0, SR01Up_Avg),
                IR01UpCo_Avg = ifelse(IR01UpCo_Avg < 0, NA, IR01UpCo_Avg)) %>%
  filter(timestamp < as_datetime("2018-11-19 00:00:00"))

hrly.flux.obs <- obs.units.match %>%
  dplyr::mutate(date = date(timestamp)) %>%
  dplyr::mutate(hour = hour(timestamp)) %>%
  dplyr::group_by(date, hour) %>%
  dplyr::summarize(SR01Up_Avg = mean(SR01Up_Avg),
                   IR01UpCo_Avg = mean(IR01UpCo_Avg)) %>%
  ungroup() %>%
  dplyr::mutate(timestamp = as_datetime(paste(date, " ", hour, ":","00:00", sep = ""), tz = "US/Eastern") + 60*60) %>% # add one hour so that timestamp represents average over past hour
  select(timestamp, SR01Up_Avg, IR01UpCo_Avg)

hrly.state.obs <- obs.units.match %>% group_by(timestamp) %>%
  dplyr::summarize(AirTK_Avg = mean(AirTK_Avg),
                   RH = mean(RH),
                   WS_ms_Avg = mean(WS_ms_Avg)) %>%
  ungroup()

hrly.obs <- inner_join(hrly.flux.obs, hrly.state.obs, by = "timestamp")


START_TIME = "2018-11-10 01:00:00"

sum.table = data_frame(metric = c("temp","RH","ws","sw","lw"),
                       r2 = rep(0,5),
                       mean.residual = rep(0,5),
                       CI.90 = rep(0,5),
                       CI.95 = rep(0,5),
                       CI.100 = rep(0,5))

mean.table = data_frame(metric = c("temp","RH","ws","sw","lw"),
                        r2 = rep(NA,5),
                        mean.residual = rep(NA,5),
                        CI.90 = rep(NA,5),
                        CI.95 = rep(NA,5),
                        CI.100 = rep(NA,5))
count = 0
for (i in 0:14){ # 15
  start_date = as_date(as_datetime(START_TIME, tz = "US/Eastern") + 24*60*60 * i)
  end_time = as_datetime(START_TIME, tz = "US/Eastern") +  24*60*60 * (i + 3)
  forcast.output = process_GEFS(start_date = start_date,
                                end_time = end_time,
                                DOWNSCALE_MET = FALSE,
                                ADD_NOISE = FALSE)
  obs.and.forecast <- inner_join(hrly.obs, forcast.output, by = "timestamp")
  table.i = compare_downscaled_to_obs(obs.and.forecast, PRINT = TRUE, PLOT = TRUE) # iterating one day at a time, 1-day intervals
  for(n in 1:nrow(table.i)){
    for(m in 2:ncol(table.i)){
      sum.table[n,m] = sum.table[n,m] + table.i[n,m]
    }
  }
  count = count + 1
}
for(n in 1:nrow(sum.table)){
  for(m in 2:ncol(sum.table)){
    mean.table[n,m] = sum.table[n,m]/count
  }
}

out.of.box.1.day = mean.table
out.of.box.1.day 
