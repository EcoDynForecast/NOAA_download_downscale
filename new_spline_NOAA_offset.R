new_spline_NOAA_offset <- function(redistributed){
  
  interpolate <- function(doy, var){
    result <- splinefun(doy, var, method = "monoH.FC")
    return(result(seq(min(doy),max(doy),1/24)))
  }
  
  # for debiased
  by.ens <- redistributed %>% 
    group_by(NOAA.member, dscale.member) %>%
    mutate(doy = yday)
  
  interp.df.doy <- by.ens %>% do(doy = seq(min(redistributed$yday), max(redistributed$yday), 1/24))
  # possibly add in a timestamp column here
  interp.df.temp <- do(by.ens, interp.temp = interpolate(.$doy,.$ds.temp))
  interp.df.ws <- do(by.ens, interp.ws = interpolate(.$doy,.$ds.ws))
  interp.df.RH <- do(by.ens, interp.RH = interpolate(.$doy,.$ds.RH))
  interp.df <- inner_join(interp.df.doy, interp.df.temp, by = c("NOAA.member","dscale.member")) %>%
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

