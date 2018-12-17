# new evaluate_downscaling.R
# purpose: call process_GEFS, compare results to obs data, make r2 table, make plots
compare_downscaled_to_obs <- function(obs.and.forecast, PRINT, PLOT){
  joined = obs.and.forecast
  mean.joined = joined %>%
    dplyr::group_by(timestamp) %>%
    dplyr::summarize(SR01Up_Avg = mean(SR01Up_Avg),
                     IR01UpCo_Avg = mean(IR01UpCo_Avg),
                     AirTK_Avg = mean(AirTK_Avg),
                     RH = mean(RH),
                     WS_ms_Avg = mean(WS_ms_Avg),
                     NOAA.member = "mean",
                     AirTemp = mean(AirTemp),
                     WindSpeed = mean(WindSpeed),
                     RelHum = mean(RelHum),
                     ShortWave = mean(ShortWave),
                     LongWave = mean(LongWave))
  summary.table = data_frame(metric = c("temp","RH","ws","sw","lw"),
                             r2 = rep(NA,5),
                             mean.residual = rep(NA,5),
                             CI.90 = rep(NA,5),
                             CI.95 = rep(NA,5),
                             CI.100 = rep(NA,5))
  
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
  
  formula = lm(mean.joined$IR01UpCo_Avg ~ mean.joined$LongWave)
  summary.table[5,2] = summary(lm(formula))$r.squared
  summary.table[5,3] = mean(mean.joined$IR01UpCo_Avg - mean.joined$LongWave, na.rm = TRUE)
  summary.table[5,4] = check_CI(df = joined, obs.col.name = "IR01UpCo_Avg", for.col.name = "LongWave")$check.90.pcnt
  summary.table[5,5] = check_CI(df = joined, obs.col.name = "IR01UpCo_Avg", for.col.name = "LongWave")$check.95.pcnt
  summary.table[5,6] = check_CI(df = joined, obs.col.name = "IR01UpCo_Avg", for.col.name = "LongWave")$check.100.pcnt
  
  if(PRINT){
    print(summary.table)
  }
  # summary.table
  if(PLOT){
    p1 <- ggplot(data = joined, aes(x = timestamp)) +
      # geom_point(aes(y = AirTemp.ds, color = "ds")) +
      geom_line(aes(y = AirTemp - 273.15, color = "Downscaled", group = interaction(NOAA.member, dscale.member)), alpha = 0.3) +
      geom_point(aes(y = AirTK_Avg - 273.15, color = "Site Observations")) + 
      geom_line(aes(y = AirTK_Avg - 273.15, color = "Site Observations")) + 
      ylab("Air Temperature (Degrees Celsius)")+
      xlab("")+
      theme_bw()+
      theme(text = element_text(size = 14)) +
      # scale_color_brewer(palette = "Dark2")
      scale_color_manual(values = c("firebrick2","black"))
    
    p2 <- ggplot(data = joined, aes(x = timestamp)) +
      geom_line(aes(y = ShortWave, color = "Downscaled",  group = interaction(NOAA.member, dscale.member)), size = 1, alpha = 0.2) + 
      geom_line(aes(y = SR01Up_Avg, color = "Site Observations"), size = 1) +
      # geom_point(aes(y = ShortWave, color = "Downscaled")) + 
      geom_point(aes(y = SR01Up_Avg, color = "Site Observations")) +
      ylab("Shortwave Radiation (W/m2)")+
      xlab("") +
      theme_bw() +
      theme(text = element_text(size = 14)) +
      scale_color_manual(values = c("firebrick2","black"))
    
    p3 <- ggplot(data = joined, aes(x = timestamp)) +
      geom_line(aes(y = LongWave, color = "Downscaled",group = interaction(NOAA.member, dscale.member)), size = 1, alpha = 0.4) + 
      geom_line(aes(y = IR01UpCo_Avg, color = "Site Observations")) + 
      ylab("Longwave Radiation (W/m2)")+
      xlab("") +
      theme_bw() +
      theme(text = element_text(size = 14)) +
      scale_color_manual(values = c("firebrick2","black"))
    
    
    p4 <- ggplot(data = joined, aes(x = timestamp)) +
      geom_line(aes(y = RelHum, color = "Downscaled", group = interaction(NOAA.member, dscale.member)), size = 1, alpha = 0.3) + 
      #geom_line(aes(y = RH, color = "Site Observations")) + 
      geom_point(aes(y = RH, color = "Site Observations")) + 
      ylab("Relative Humidity (%)")+
      xlab("")+
      theme(text = element_text(size = 14)) +
      scale_color_brewer(palette = "Set1")
    
    p5 <- ggplot(data = joined, aes(x = timestamp)) +
      geom_line(aes(y = WindSpeed, color = "Downscaled", group = interaction(NOAA.member, dscale.member)), size = 1, alpha = 0.4) + 
      geom_line(aes(y = WS_ms_Avg, color = "Site Observations")) + 
      # geom_point(aes(y = WindSpeed, color = "Downscaled")) + 
      geom_point(aes(y = WS_ms_Avg, color = "Site Observations")) + 
      ylab("Wind Speed (m/s)")+
      xlab("")+
      theme(text = element_text(size = 14)) +
      scale_color_brewer(palette = "Set1")
    print(p1)
    print(p2)
    print(p3)
    print(p4)
    print(p5)
  }
  return(summary.table)
}
