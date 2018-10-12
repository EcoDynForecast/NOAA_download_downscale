plot_spline <- function(expanded.forecast.units.match, obs.units.match, expanded.debiased.with.noise, debiased.downscaled, joined.obs.and.ds, joined.obs.and.NOAA, start_day, num_days){
  
  end_day = start_day + num_days
  tmp.obs <- obs.units.match %>% filter(doy >= start_day & doy <= end_day)
  tmp.NOAA <-expanded.forecast.units.match %>%
    filter(doy >= start_day & doy <= end_day) %>%
    arrange(group.num)
  
  tmp.NOAA.interval = tmp.NOAA %>%
    group_by(doy) %>%
    dplyr::summarize(temp.min = min(temp),
                  RH.min = min(RH),
                  ws.min = min(ws),
                  temp.mean = mean(temp),
                  temp.max = max(temp),
                  RH.max = max(RH),
                  ws.max = max(ws))
  
  tmp.expanded.debiased <- expanded.debiased.with.noise %>% filter(doy >= start_day & doy <= end_day) %>% arrange(NOAA.member, doy, dscale.member)
  tmp.debiased.downscaled <- debiased.downscaled %>% filter(doy >= start_day & doy <= end_day)
  
  tmp.debiased.downscaled.interval = tmp.debiased.downscaled %>%
    group_by(doy) %>%
    dplyr::summarize(temp.min = min(interp.temp),
                     RH.min = min(interp.RH),
                     ws.min = min(interp.ws),
                     temp.mean = mean(interp.temp),
                     temp.max = max(interp.temp),
                     RH.max = max(interp.RH),
                     ws.max = max(interp.ws))
  tmp.joined.obs.and.ds <- joined.obs.and.ds  %>% filter(doy >= start_day & doy <= end_day)
  tmp.joined.obs.and.NOAA <-joined.obs.and.NOAA %>% filter(doy >= start_day & doy <= end_day)
  alpha = 0.5
  
# interp.temp
plot1.a <- 
  ggplot() +
  geom_line(data = tmp.obs, aes(doy, AirTC_Avg), col = "black", alpha = alpha) +
  geom_ribbon(data = tmp.NOAA.interval, aes(doy, ymin = temp.min, ymax = temp.max), fill = "blue", alpha = alpha) +
  geom_line(data = tmp.NOAA.interval, aes(doy, temp.mean), col = "blue", alpha = alpha) +
 #  geom_line(data = tmp.expanded.debiased %>% group_by(NOAA.member, dscale.member), aes(x = doy, y = temp.mod.noise, group = interaction(NOAA.member, dscale.member)), col = "red", alpha = alpha) +
  geom_line(data = tmp.debiased.downscaled.interval, aes(doy, temp.mean), col = "red", alpha = alpha) +
  geom_point(data = tmp.debiased.downscaled.interval, aes(doy, temp.mean), col = "red", alpha = alpha) +
  geom_ribbon(data = tmp.debiased.downscaled.interval, aes(doy, ymin = temp.min, ymax = temp.max), fill = "red", alpha = alpha) 
  
print(plot1.a)

  # plot1.b <- ggplot() +
  #   geom_point(data = tmp.joined.obs.and.ds, aes(AirTC_Avg, interp.temp), col = "red", alpha = alpha) +
  #   geom_point(data =tmp.joined.obs.and.NOAA, aes(AirTC_Avg, temp), col = "black", alpha = alpha) +
  #   geom_abline(intercept = 0, slope = 1, col = "blue")
  #   print(plot1.a)
  #  # print(plot1.b)
  #  
  #  #grid.arrange(plot1.a, plot1.b, ncol = 2)
  # 
  #  # plot1.d <- ggplot() +
  #  #   geom_point(data = tmp.joined.obs.and.ds, aes(x = AirTC_Avg, y = interp.temp -AirTC_Avg ) )
  #  # 
  #  # 
  # plot1.c <- ggplot() +
  #    geom_density(data = tmp.joined.obs.and.ds, mapping = aes(x = AirTC_Avg - interp.temp, y = (..count..)/sum(..count..)), fill = "red", alpha = alpha) + 
  #    geom_density(data = tmp.joined.obs.and.NOAA, aes(x = AirTC_Avg -  temp, y = (..count..)/sum(..count..)), fill = "black", alpha = alpha)
  #print(plot1.c)
  # 
## wind speed  
#   plot2.a <- ggplot() +
#     geom_line(data = tmp.obs, aes(doy, WS_ms_Avg), col = "black", alpha = alpha)+
#     geom_line(data = tmp.NOAA, aes(doy, ws, group =NOAA.member), col = "blue", alpha = alpha) +
#     geom_line(data = tmp.expanded.debiased, aes(doy, ws.mod, group =NOAA.member), col = "red", alpha = alpha) +
#     geom_line(data = tmp.debiased.downscaled, aes(doy, interp.ws, group =NOAA.member), col = "green", alpha = alpha)
#   scale_color_manual(values= colors, labels = c("four", "six", "eight")) 
#   
#   
#   plot2.b <- ggplot() +
#     geom_point(data = joined.obs.and.ds, aes(WS_ms_Avg, interp.ws), col = "black", alpha = alpha) +
#     geom_abline(intercept = 0, slope = 1, col = "blue")
#   # print(plot2.a)
#   # print(plot2.b)
#   # grid.arrange(plot2.a, plot2.b, ncol = 2)
#   
# ## relative humidity  
#   plot3.a <- ggplot() +
#     geom_line(data = tmp.obs, aes(doy, RH), col = "black", alpha = alpha) +
#     geom_line(data = tmp.NOAA, aes(doy, RH, group =NOAA.member), col = "blue", alpha = alpha) +
#     geom_line(data = tmp.expanded.debiased, aes(doy, RH.mod, group =NOAA.member), col = "red", alpha = alpha) +
#     geom_line(data = tmp.debiased.downscaled, aes(doy, interp.RH, group =NOAA.member), col = "green", alpha = alpha)
#   scale_color_manual(values= colors, labels = c("four", "six", "eight")) 
# 
#   plot3.b <- ggplot() +
#     geom_point(data = joined.obs.and.ds, aes(RH, interp.RH), col = "black", alpha = alpha) +
#     geom_abline(intercept = 0, slope = 1, col = "blue")
# 
# # print(plot3.a)
# # print(plot3.b)
# # grid.arrange(plot3.a, plot3.b, ncol = 2)
}
