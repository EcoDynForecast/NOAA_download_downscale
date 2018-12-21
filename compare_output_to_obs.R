compare_output_to_obs <- function(output, obs.file.path, PLOT){
  # read in obs data
  obs.data <- read.csv(file = obs.file.path, header = TRUE)
  observations = prep_obs(obs.data) %>%
    # max air temp record in Vinton, VA is 40.6 C 
    # coldest air temp on record in Vinton, Va is -23.9 C
    # http://www.climatespy.com/climate/summary/united-states/virginia/roanoke-regional 
    # lots of bad data for longwave between 8/23/2018 and 9/11/2018 randomly for a couple minutes at a       # time. Removing entire section of data for now. Also bad data on a few other days
    dplyr::mutate(AirTemp = ifelse(AirTemp> 273.15 + 41, NA, AirTemp),
                  AirTemp = ifelse(AirTemp < 273.15 -23.9, NA, AirTemp),
                  ShortWave = ifelse(ShortWave < 0, 0, ShortWave),
                  LongWave = ifelse(LongWave < 0, NA, LongWave),
                  AirTemp = AirTemp - 273.15) %>%
    filter(timestamp >= min(output$timestamp) & timestamp <= max(output$timestamp))
  
  hrly.obs = aggregate_obs_to_hrly(observations)
  joined = full_join(hrly.obs, output, by = "timestamp", suffix = c(".obs",".for"))
  mean.joined = joined %>%
    group_by(timestamp) %>%
    summarize_all("mean")
  
  ## make a summary table of comparison between output and observations
  summary.table = data_frame(metric = c("AirTemp","RelHum","WindSpeed","ShortWave","LongWave"),
                             r2 = rep(NA,5),
                             mean.residual = rep(NA,5),
                             CI.90 = rep(NA,5),
                             CI.95 = rep(NA,5),
                             CI.100 = rep(NA,5))
  
  formula = lm(mean.joined$AirTemp.obs ~ mean.joined$AirTemp.for)
  summary.table[1,2] = summary(lm(formula))$r.squared
  summary.table[1,3] = mean(mean.joined$AirTemp.obs - mean.joined$AirTemp.for, na.rm = TRUE)
  summary.table[1,4] = check_CI(df = joined, obs.col.name = "AirTemp.obs", for.col.name = "AirTemp.for")$check.90.pcnt
  summary.table[1,5] = check_CI(df = joined, obs.col.name = "AirTemp.obs", for.col.name = "AirTemp.for")$check.95.pcnt
  summary.table[1,6] = check_CI(df = joined, obs.col.name = "AirTemp.obs", for.col.name = "AirTemp.for")$check.100.pcnt
  
  formula = lm(mean.joined$RelHum.obs ~ mean.joined$RelHum.for)
  summary.table[2,2] = summary(lm(formula))$r.squared
  summary.table[2,3] = mean(mean.joined$RelHum.obs - mean.joined$RelHum.for, na.rm = TRUE)
  summary.table[2,4] = check_CI(df = joined, obs.col.name = "RelHum.obs", for.col.name = "RelHum.for")$check.90.pcnt
  summary.table[2,5] = check_CI(df = joined, obs.col.name = "RelHum.obs", for.col.name = "RelHum.for")$check.95.pcnt
  summary.table[2,6] = check_CI(df = joined, obs.col.name = "RelHum.obs", for.col.name = "RelHum.for")$check.100.pcnt
  
  formula = lm(mean.joined$WindSpeed.obs  ~ mean.joined$WindSpeed.for)
  summary.table[3,2] = summary(lm(formula))$r.squared
  summary.table[3,3] = mean(mean.joined$WindSpeed.obs -  mean.joined$WindSpeed.for, na.rm = TRUE)
  summary.table[3,4] = check_CI(df = joined, obs.col.name = "WindSpeed.obs", for.col.name = "WindSpeed.for")$check.90.pcnt
  summary.table[3,5] = check_CI(df = joined, obs.col.name = "WindSpeed.obs", for.col.name = "WindSpeed.for")$check.95.pcnt
  summary.table[3,6] = check_CI(df = joined, obs.col.name = "WindSpeed.obs", for.col.name = "WindSpeed.for")$check.100.pcnt
  
  formula = lm(mean.joined$ShortWave.obs ~ mean.joined$ShortWave.for)
  summary.table[4,2] = summary(lm(formula))$r.squared
  summary.table[4,3] = mean(mean.joined$ShortWave.obs -  mean.joined$ShortWave.for, na.rm = TRUE)
  summary.table[4,4] = check_CI(df = joined, obs.col.name = "ShortWave.obs", for.col.name = "ShortWave.for")$check.90.pcnt
  summary.table[4,5] = check_CI(df = joined, obs.col.name = "ShortWave.obs", for.col.name = "ShortWave.for")$check.95.pcnt
  summary.table[4,6] = check_CI(df = joined, obs.col.name = "ShortWave.obs", for.col.name = "ShortWave.for")$check.100.pcnt
  
  formula = lm(mean.joined$LongWave.obs ~ mean.joined$LongWave.for)
  summary.table[5,2] = summary(lm(formula))$r.squared
  summary.table[5,3] = mean(mean.joined$LongWave.obs - mean.joined$LongWave.for, na.rm = TRUE)
  summary.table[5,4] = check_CI(df = joined, obs.col.name = "LongWave.obs", for.col.name = "LongWave.for")$check.90.pcnt
  summary.table[5,5] = check_CI(df = joined, obs.col.name = "LongWave.obs", for.col.name = "LongWave.for")$check.95.pcnt
  summary.table[5,6] = check_CI(df = joined, obs.col.name = "LongWave.obs", for.col.name = "LongWave.for")$check.100.pcnt

  if(PLOT){
    print(ggplot(data = joined, aes(x = timestamp)) +
      geom_line(aes(y = AirTemp.for, color = "Downscaled", group = interaction(NOAA.member, dscale.member)), alpha = 0.3) +
      geom_point(aes(y = AirTemp.obs, color = "Site Observations")) + 
      geom_line(aes(y = AirTemp.obs, color = "Site Observations")) + 
      ylab("Air Temperature (Degrees Celsius)")+
      xlab("")+
      theme_bw()+
      theme(text = element_text(size = 14)) +
      scale_color_manual(values = c("firebrick2","black")))
    
    print(ggplot(data = joined, aes(x = timestamp)) +
      geom_line(aes(y = ShortWave.for, color = "Downscaled", group = interaction(NOAA.member, dscale.member)), alpha = 0.3) +
      geom_line(aes(y = ShortWave.obs, color = "Site Observations")) + 
      ylab("Shortwave Radiation (W/m2)")+
      xlab("")+
      theme_bw()+
      theme(text = element_text(size = 14)) +
      scale_color_manual(values = c("firebrick2","black")))
    
    print(ggplot(data = joined, aes(x = timestamp)) +
      geom_line(aes(y = LongWave.for, color = "Downscaled", group = interaction(NOAA.member, dscale.member)), alpha = 0.4) +
      geom_line(aes(y = LongWave.obs, color = "Site Observations")) + 
      ylab("Longwave Radiation (W/m2)")+
      xlab("")+
      theme_bw()+
      theme(text = element_text(size = 14)) +
      scale_color_manual(values = c("firebrick2","black")))
    
    print(ggplot(data = joined, aes(x = timestamp)) +
      geom_line(aes(y = RelHum.for, color = "Downscaled", group = interaction(NOAA.member, dscale.member)), alpha = 0.3) +
      geom_point(aes(y = RelHum.obs, color = "Site Observations")) + 
      geom_line(aes(y = RelHum.obs, color = "Site Observations")) + 
      ylab("Relative Humidity (%)")+
      xlab("")+
      theme_bw()+
      theme(text = element_text(size = 14)) +
      scale_color_manual(values = c("firebrick2","black")))
    
    print(ggplot(data = joined, aes(x = timestamp)) +
      geom_line(aes(y = WindSpeed.for, color = "Downscaled", group = interaction(NOAA.member, dscale.member)), alpha = 0.3) +
      geom_point(aes(y = WindSpeed.obs, color = "Site Observations")) + 
      geom_line(aes(y = WindSpeed.obs, color = "Site Observations")) + 
      ylab("Wind Speed (m/s)")+
      xlab("")+
      theme_bw()+
      theme(text = element_text(size = 14)) +
      scale_color_manual(values = c("firebrick2","black")))
  }
  return(summary.table)
}
