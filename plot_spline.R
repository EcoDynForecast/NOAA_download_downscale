plot_spline <- function(forecast.units.match,obs.units.match,debiased, debiased.downscaled, start_day, num_days){
  end_day = start_day + num_days
  
  tmp.obs <- obs.units.match %>% filter(doy >= start_day & doy <= end_day)
  tmp.NOAA <-forecast.units.match %>%
    filter(doy >= start_day & doy <= end_day) %>%
    arrange(group.num, doy)
  tmp.debiased <- debiased %>% filter(doy >= start_day & doy <= end_day)
  tmp.debiased.downscaled <- debiased.downscaled %>% filter(doy >= start_day & doy <= end_day)
  
  alpha = 0.7
  ## interp.temp
  plot1 <- ggplot() +
    geom_line(data = tmp.obs, aes(doy, AirTC_Avg), col = "black", alpha = alpha)+
    geom_line(data = tmp.NOAA, aes(doy, temp, group = NOAA.member), col = "blue", alpha = alpha) +
    geom_line(data = tmp.debiased, aes(doy, temp.mod, group =NOAA.member), col = "red", alpha = alpha) + 
    geom_line(data = tmp.debiased.downscaled, aes(doy, interp.temp, group =NOAA.member), col = "green", alpha = alpha)
  print(plot1)
  
  ## wind speed  
  plot2 <- ggplot() +
    geom_line(data = tmp.obs, aes(doy, WS_ms_Avg), col = "black", alpha = alpha)+
    geom_line(data = tmp.NOAA, aes(doy, ws, group =NOAA.member), col = "blue", alpha = alpha) +
    geom_line(data = tmp.debiased, aes(doy, ws.mod, group =NOAA.member), col = "red", alpha = alpha) +
    geom_line(data = tmp.debiased.downscaled, aes(doy, interp.ws, group =NOAA.member), col = "green", alpha = alpha)
  scale_color_manual(values= colors, labels = c("four", "six", "eight")) 
  print(plot2)
  
  ## relative humidity  
  plot3 <- ggplot() +
    geom_line(data = tmp.obs, aes(doy, RH), col = "black", alpha = alpha) +
    geom_line(data = tmp.NOAA, aes(doy, RH, group =NOAA.member), col = "blue", alpha = alpha) +
    geom_line(data = tmp.debiased, aes(doy, RH.mod, group =NOAA.member), col = "red", alpha = alpha) +
    geom_line(data = tmp.debiased.downscaled, aes(doy, interp.RH, group =NOAA.member), col = "green", alpha = alpha)
  scale_color_manual(values= colors, labels = c("four", "six", "eight")) 
  print(plot3)
}
