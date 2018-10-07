spline_NOAA <- function(debiased){
  # # repeat NOAA values over past 6 hours
  # debiased[,"group.num"] = row(debiased)[,1] # create a group number for each original NOAA forecast entry
  # debiased <- debiased %>%
  #   group_by(NOAA.member, group.num, doy, temp.mod, ws.mod, RH.mod) %>%
  #   tidyr::expand(doy = c(doy - 6/24,doy - 5/24,doy - 4/24,doy - 3/24,doy - 2/24,doy - 1/24, doy)) %>%
  #   ungroup() 
  # 
  
  ### interpolating sw
  
  interpolate <- function(doy, var){
    result <- splinefun(doy, var, method = "monoH.FC")
    return(result(seq(min(doy),max(doy),1/24)))
  }
  
  by.ens <- debiased %>% 
    group_by(NOAA.member)
  
  interp.df.doy <- by.ens %>% do(doy = seq(min(debiased$doy), max(debiased$doy), 1/24))
  interp.df.temp <- do(by.ens, interp.temp = interpolate(.$doy,.$temp.mod))
  interp.df.ws <- do(by.ens, interp.ws = interpolate(.$doy,.$ws.mod))
  interp.df.RH <- do(by.ens, interp.RH = interpolate(.$doy,.$RH.mod))
  interp.df <- inner_join(interp.df.doy, interp.df.temp, by = c("NOAA.member")) %>%
    inner_join(interp.df.ws, by = c("NOAA.member")) %>%
    inner_join(interp.df.RH,  by = c("NOAA.member")) %>%
    unnest()
  #interp.df <- interp.df %>%
  #  inner_join(debiased %>% select(doy), by = "doy")
  return(interp.df)
}

