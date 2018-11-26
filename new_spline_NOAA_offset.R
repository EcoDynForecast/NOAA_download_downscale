new_spline_NOAA_offset <- function(redistributed){
  
  interpolate <- function(jday, var){
    result <- splinefun(jday, var, method = "monoH.FC")
    return(result(seq(min(as.numeric(jday)), max(as.numeric(jday)), 1/24)))
  }
  
  # for debiased
  by.ens <- redistributed %>% 
    group_by(NOAA.member, dscale.member) %>%
    mutate(jday = julian(timestamp, origin = "1970-01-01 00:00:00"))
  
  interp.df.jday <- by.ens %>% do(jday = seq(as.numeric(min(julian(redistributed$timestamp, origin = "1970-01-01 00:00:00"))), as.numeric(max(julian(redistributed$timestamp, origin = "1970-01-01 00:00:00"))), 1/24))
  # possibly add in a timestamp column here
  interp.df.temp <- do(by.ens, interp.temp = interpolate(.$jday,.$ds.temp))
  interp.df.ws <- do(by.ens, interp.ws = interpolate(.$jday,.$ds.ws))
  interp.df.RH <- do(by.ens, interp.RH = interpolate(.$jday,.$ds.RH))
  interp.df <- inner_join(interp.df.jday, interp.df.temp, by = c("NOAA.member","dscale.member")) %>%
    inner_join(interp.df.ws, by = c("NOAA.member","dscale.member")) %>%
    inner_join(interp.df.RH,  by = c("NOAA.member","dscale.member")) %>%
    unnest()
  
  # for debiased.with.noise
  
  # by.ens.noise <- debiased.with.noise %>% 
  #   group_by(NOAA.member, dscale.member)
  # 
  # interp.df.doy.noise <- by.ens.noise %>% do(doy = seq(min(debiased.with.noise$doy), max(debiased.with.noise$doy), 1/24))
  # # possibly add in a timestamp column here
  # interp.df.temp.noise <- do(by.ens.noise, interp.temp.noise = interpolate(.$doy,.$temp.noise))
  # interp.df.ws.noise <- do(by.ens.noise, interp.ws.noise = interpolate(.$doy,.$ws.noise))
  # interp.df.RH.noise <- do(by.ens.noise, interp.RH.noise = interpolate(.$doy,.$RH.noise))
  # interp.df.noise <- inner_join(interp.df.doy.noise, interp.df.temp.noise, by = c("NOAA.member","dscale.member")) %>%
  #   inner_join(interp.df.ws.noise, by = c("NOAA.member","dscale.member")) %>%
  #   inner_join(interp.df.RH.noise,  by = c("NOAA.member","dscale.member")) %>%
  #   unnest()
  # #interp.df <- interp.df %>%
  # #  inner_join(debiased.with.noise %>% select(doy), by = "doy")
  return(interp.df)
}

