debias_and_add_error <- function(joined.6.hourly){
  # rm(list = ls())
  # library(lubridate)
  # library(dplyr)
  # library(tidyr)
  # library(ggpmisc)
  # library(gridExtra)
  # nmembers = 20
  # path.working <- "/Users/laurapuckett/Documents/Research/Fall 2018/"
  # joined.6.hourly = readRDS(file = paste(path.working, "my_files/","joined.6.hourly", sep = "")) %>%
  #   rename(c("ensembles" = "NOAA.member"))
  
  # will need to aggregate shortwave to daily, compare to obs, then apply equations to get back to hourly at temporal downscaling steps. Mike Dietze has done this already - might have to track it down. (Prediction Ecology, a First Principles Framework) Look up Solar Geometry. 
  # most vars will be interpolated for temporal downscaling
  # 
  # End Goal: plot NOAA ensembles vs downscaled NOAA ensemble members
  
  # don't take ensemble mean - do everything for each ensemble (completed 9-26)
  
  lin.mod <- function(col.obs, col.for){
    model = lm(col.obs ~ col.for)
    slope = model$coefficients[2]
    intercept = model$coefficients[1]
    modeled = col.for*slope + intercept
    return(modeled)
  }
  lm.res.sd <- function(col.obs, col.for){
    model = lm(col.obs ~ col.for)
    res.sd = sd(residuals(model))
    return(res.sd)
  }
  
  temp.res.sd = lm.res.sd(joined.6.hourly$temp.obs, joined.6.hourly$temp.for)
  RH.res.sd = lm.res.sd(joined.6.hourly$RH.obs, joined.6.hourly$RH.for)
  avg.sw.res.sd = lm.res.sd(joined.6.hourly$avg.sw.obs, joined.6.hourly$avg.sw.for)
  avg.lw.res.sd = lm.res.sd(joined.6.hourly$avg.lw.obs, joined.6.hourly$avg.lw.for)
  precip.rate.res.sd = lm.res.sd(joined.6.hourly$precip.rate.obs, joined.6.hourly$precip.rate.for)
  avg.ws.res.sd = lm.res.sd(joined.6.hourly$avg.ws.obs, joined.6.hourly$avg.ws.for)
  
  
  debiased <- joined.6.hourly %>% 
    mutate(temp.mod =  lin.mod(joined.6.hourly$temp.obs, joined.6.hourly$temp.for),
           RH.mod =  lin.mod(joined.6.hourly$RH.obs, joined.6.hourly$RH.for),
           avg.sw.mod =  lin.mod(joined.6.hourly$avg.sw.obs, joined.6.hourly$avg.sw.for),
           avg.lw.mod =  lin.mod(joined.6.hourly$avg.lw.obs, joined.6.hourly$avg.lw.for),
           precip.rate.mod =  lin.mod(joined.6.hourly$precip.rate.obs, joined.6.hourly$precip.rate.for),
           avg.ws.mod =  lin.mod(joined.6.hourly$avg.ws.obs, joined.6.hourly$avg.ws.for)) %>%
    select(timestamp, NOAA.member, temp.mod, RH.mod, avg.sw.mod, avg.lw.mod, precip.rate.mod, avg.ws.mod)
  
  
  debias.with.noise <- debiased %>% 
    group_by(timestamp, NOAA.member, temp.mod, RH.mod, avg.sw.mod, avg.lw.mod, precip.rate.mod, avg.ws.mod) %>%
    expand(dscale.member = 1:nmembers) %>% group_by(timestamp) %>%
    mutate(temp = temp.mod + rnorm(mean = 0, sd = temp.res.sd, n = nmembers),
           RH = RH.mod + rnorm(mean = 0, sd = RH.res.sd, n = nmembers),
           avg.sw = avg.sw.mod + rnorm(mean = 0, sd = avg.sw.res.sd, n = nmembers),
           avg.lw = avg.lw.mod + rnorm(mean = 0, sd = avg.lw.res.sd, n = nmembers),
           precip.rate = precip.rate.mod + rnorm(mean = 0, sd = precip.rate.res.sd, n = nmembers),
           avg.ws = avg.ws.mod + rnorm(mean = 0, sd = avg.ws.res.sd, n = nmembers)) # %>%
  # group_by(timestamp) %>% # TO CHECK ENSEMBLES
  # dplyr::summarize(mod.temp = mean(temp.mod), 
  #          mean.ensemble = mean(temp))
  
  
  saveRDS(debias.with.noise, file = paste(path.working, "/debias.with.noise",sep = ""))
}
