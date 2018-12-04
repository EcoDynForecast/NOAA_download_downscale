assess_downscaling <- function(start_time, end_time, DOWNSCALE_MET, ADD_NOISE, PLOT, PRINT){
  # reruns at each function call to get up to date ds_output and ds_ouput_no_noise files
  
  ## OBSERVATIONAL DATA
  obs.data <- read.csv(paste('/Users/laurapuckett/Documents/Research/Fall 2018/', "my_files/", "FCRmet.csv", sep = ""), header = TRUE)
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
  
  ## GET FORECAST DATA
  file_name = paste(year(start_time), 
                    ifelse(month(start_time)<10,
                           paste("0",month(start_time),sep = ""),
                           month(start_time)),
                    ifelse(day(start_time)<10,
                           paste("0",day(start_time),sep = ""),
                           day(start_time)),
                    "gep_all_00z", sep = "")
  process_GEFS2GLM_v2(file_name, DOWNSCALE_MET = DOWNSCALE_MET, ADD_NOISE = ADD_NOISE, PLOT = PLOT)
  if(DOWNSCALE_MET){
    if(ADD_NOISE){
      load(file = '/Users/laurapuckett/Documents/Research/Fall 2018/my_files/ds_output.RData')
      output = ds_output
    }else{
      load(file = '/Users/laurapuckett/Documents/Research/Fall 2018/my_files/ds_output_no_noise.RData')
      output = ds_output_no_noise
    }
    output <- output %>% plyr::rename(c("full_time" = "timestamp")) %>%
      mutate(timestamp = as_datetime(paste(timestamp, ':00', sep = "")))
  }else{
    full_time.df = as.data.frame(full_time)
    LongWave.df = as.data.frame(LongWave) %>% cbind(full_time.df) %>%
      gather(NOAA.member, LongWave, V1:V21)
    # Rain.df = as.data.frame(Rain)  %>% cbind(full_time.df) %>%
    #   gather(NOAA.member, Rain, V1:V21)
    # Snow.df = as.data.frame(Snow)  %>% cbind(full_time.df) %>%
    #   gather(NOAA.member, Snow, V1:V21)
    AirTemp.df = as.data.frame(AirTemp) %>% cbind(full_time.df) %>%
      gather(NOAA.member, AirTemp, V1:V21)
    WindSpeed.df = as.data.frame(WindSpeed)  %>% cbind(full_time.df) %>%
      gather(NOAA.member, WindSpeed, V1:V21)
    RelHum.df = as.data.frame(RelHum)  %>% cbind(full_time.df) %>%
      gather(NOAA.member, RelHum, V1:V21)
    ShortWave.df = as.data.frame(ShortWave) %>% cbind(full_time.df) %>%
      gather(NOAA.member, ShortWave, V1:V21)
    
    output = LongWave.df %>%
      inner_join(AirTemp.df, by = c("full_time","NOAA.member")) %>%
      inner_join(WindSpeed.df, by = c("full_time","NOAA.member")) %>%
      inner_join(RelHum.df, by = c("full_time","NOAA.member")) %>%
      inner_join(ShortWave.df, by = c("full_time","NOAA.member")) %>%
      dplyr::mutate(NOAA.member = as.integer(str_replace(NOAA.member, "V",""))) %>%
      plyr::rename(c("full_time" = "timestamp")) %>%
        dplyr::mutate(timestamp = as_datetime(paste(timestamp, ":00", sep = "")))
  }
  summary.table = data_frame(metric = c("temp","RH","ws","sw","lw"),
                             r2 = rep(NA,5),
                             mean.residual = rep(NA,5),
                             CI.90 = rep(NA,5),
                             CI.95 = rep(NA,5),
                             CI.100 = rep(NA,5))
  
  joined <- dplyr::inner_join(obs.units.match, output, by = "timestamp") %>% group_by(timestamp, NOAA.member) %>%
    dplyr::summarize(AirTemp = first(AirTemp), # doing this bc join function misbehaving and producing replicates
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
    dplyr::mutate(daily_IR01DnCo_Avg = mean(IR01DnCo_Avg)) %>% # daily average shortwave
    ungroup()
  
  mean.joined <- joined %>%
    dplyr::group_by(timestamp) %>% # average across ensembles
    dplyr::summarize(WindSpeed = mean(WindSpeed),
                     RelHum = mean(RelHum),
                     ShortWave = mean(ShortWave),
                     AirTemp = mean(AirTemp),
                     # LongWave = mean(LongWave), # come back to longwave later
                     AirTK_Avg = mean(AirTK_Avg),
                     RH = mean(RH),
                     SR01Up_Avg = mean(SR01Up_Avg),
                     WS_ms_Avg = mean(WS_ms_Avg),
                     IR01DnCo_Avg = mean(IR01DnCo_Avg)) %>%
    ungroup()
  
  formula = lm(mean.joined$AirTK_Avg ~ mean.joined$AirTemp)
  summary.table[1,2] = summary(lm(formula))$r.squared
  summary.table[1,3] = mean(mean.joined$AirTK_Avg - mean.joined$AirTemp, na.rm = TRUE)
  summary.table[1,4] = check_CI(df = joined, obs.col.name = "AirTK_Avg", for.col.name = "AirTemp")$check.90.pcnt
  summary.table[1,5] = check_CI(df = joined, obs.col.name = "AirTK_Avg", for.col.name = "AirTemp")$check.95.pcnt
  summary.table[1,6] = check_CI(df = joined, obs.col.name = "AirTK_Avg", for.col.name = "AirTemp")$check.100.pcnt
  
  formula = lm(mean.joined$RH ~ mean.joined$RelHum)
  summary.table[2,2] = summary(lm(formula))$r.squared
  summary.table[2,3] = mean(mean.joined$RH - mean.joined$RelHum, na.rm = TRUE)
  summary.table[2,4] = check_CI(df = joined, obs.col.name = "RH", for.col.name = "RelHum")$check.90.pcnt
  summary.table[2,5] = check_CI(df = joined, obs.col.name = "RH", for.col.name = "RelHum")$check.95.pcnt
  summary.table[2,6] = check_CI(df = joined, obs.col.name = "RH", for.col.name = "RelHum")$check.100.pcnt
  
  formula = lm(mean.joined$WS_ms_Avg  ~ mean.joined$WindSpeed)
  summary.table[3,2] = summary(lm(formula))$r.squared
  summary.table[3,3] = mean(mean.joined$WS_ms_Avg -  mean.joined$WindSpeed, na.rm = TRUE)
  summary.table[3,4] = check_CI(df = joined, obs.col.name = "WS_ms_Avg", for.col.name = "WindSpeed")$check.90.pcnt
  summary.table[3,5] = check_CI(df = joined, obs.col.name = "WS_ms_Avg", for.col.name = "WindSpeed")$check.95.pcnt
  summary.table[3,6] = check_CI(df = joined, obs.col.name = "WS_ms_Avg", for.col.name = "WindSpeed")$check.100.pcnt
  
  formula = lm(mean.joined$SR01Up_Avg ~ mean.joined$ShortWave)
  summary.table[4,2] = summary(lm(formula))$r.squared
  summary.table[4,3] = mean(mean.joined$SR01Up_Avg -  mean.joined$ShortWave, na.rm = TRUE)
  summary.table[4,4] = check_CI(df = joined, obs.col.name = "SR01Up_Avg", for.col.name = "ShortWave")$check.90.pcnt
  summary.table[4,5] = check_CI(df = joined, obs.col.name = "SR01Up_Avg", for.col.name = "ShortWave")$check.95.pcnt
  summary.table[4,6] = check_CI(df = joined, obs.col.name = "SR01Up_Avg", for.col.name = "ShortWave")$check.100.pcnt
  
  # formula = lm(joined.no.noise$daily_IR01DnCo_Avg ~ joined.no.noise$LongWave)
  # summary.table[5,2] = summary(lm(formula))$r.squared
  # summary.table[5,3] = mean(lm(formula)$residuals)
  # summary.table[5,4] = check_CI(df = joined, obs.col.name = "daily_IR01DnCo_Avg", for.col.name = "LongWave")$check.90.pcnt
  # summary.table[5,5] = check_CI(df = joined, obs.col.name = "daily_IR01DnCo_Avg", for.col.name = "LongWave")$check.95.pcnt
  # summary.table[5,6] = check_CI(df = joined, obs.col.name = "daily_IR01DnCo_Avg", for.col.name = "LongWave")$check.100.pcnt
  if(PRINT){
    print(summary.table)
  }
  # summary.table
  if(PLOT){
    print(ggplot(data = mean.joined, aes(x = timestamp), alpha = 0.5)+
            # geom_point(aes(y = AirTemp.ds, color = "ds")) +
            geom_point(aes(y = AirTemp - 273.15, color = "ds")) +
            geom_point(aes(y = AirTK_Avg - 273.15, color = "obs")))
    print(ggplot(data = mean.joined, aes(x = timestamp)) +
            geom_line(aes(y = ShortWave, color = "ds")) + 
            geom_line(aes(y = SR01Up_Avg, color = "obs")))
  }
  return(summary.table)
}

