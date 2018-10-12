debias_and_add_error <- function(joined.data, nmembers){
  # rm(list = ls())
  # library(lubridate)
  # library(dplyr)
  # library(tidyr)
  # library(ggpmisc)
  # library(gridExtra)
  # nmembers = 20
  # path.working <- "/Users/laurapuckett/Documents/Research/Fall 2018/"
  # joined.data = readRDS(file = paste(path.working, "my_files/","joined.data", sep = "")) ]
  
. 
  # most vars will be interpolated for temporal downscaling
  # 
  # End Goal: plot NOAA ensembles vs downscaled NOAA ensemble members

  # WORK LOG
  # develop workflow for applying models to forecast array, creating ensembles, and adding noise (9-24, 2 hours) 
  # don't take ensemble mean - do everything for each ensemble (completed 9-26, 30 minutes)
  # organize code into functions and clean up - 30 minutes (9-26)
  
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
  
  temp.res.sd = lm.res.sd(joined.data$temp.obs, joined.data$temp.for)
  RH.res.sd = lm.res.sd(joined.data$RH.obs, joined.data$RH.for)
  #avg.sw.res.sd = lm.res.sd(joined.data$avg.sw.obs, joined.data$avg.sw.for)
  #avg.lw.res.sd = lm.res.sd(joined.data$avg.lw.obs, joined.data$avg.lw.for)
  #precip.rate.res.sd = lm.res.sd(joined.data$precip.rate.obs, joined.data$precip.rate.for)
  ws.res.sd = lm.res.sd(joined.data$ws.obs, joined.data$ws.for)
  
  
  debiased <- joined.data %>% 
    dplyr::mutate(temp.mod =  lin.mod(joined.data$temp.obs, joined.data$temp.for),
           RH.mod =  lin.mod(joined.data$RH.obs, joined.data$RH.for),
           #avg.sw.mod =  lin.mod(joined.data$avg.sw.obs, joined.data$avg.sw.for),
           #avg.lw.mod =  lin.mod(joined.data$avg.lw.obs, joined.data$avg.lw.for),
           #precip.rate.mod =  lin.mod(joined.data$precip.rate.obs, joined.data$precip.rate.for),
           ws.mod =  lin.mod(joined.data$ws.obs, joined.data$ws.for)) %>%
    select(timestamp, group.num, doy, NOAA.member, temp.mod, RH.mod, ws.mod)
  

  debiased.with.noise <- debiased %>%
    group_by(timestamp, group.num, doy, NOAA.member, temp.mod, RH.mod,  ws.mod) %>%
    expand(dscale.member = 1:nmembers) %>%
    ungroup() %>%
    group_by(timestamp, dscale.member, group.num, doy, NOAA.member, temp.mod, RH.mod,  ws.mod) %>%
    dplyr::mutate(temp.mod.noise = temp.mod + rnorm(mean = 0, sd = temp.res.sd, n = 1),
           RH.mod.noise = RH.mod + rnorm(mean = 0, sd = RH.res.sd, n = 1),
           #avg.sw = avg.sw.mod + rnorm(mean = 0, sd = avg.sw.res.sd, n = nmembers),
           #avg.lw = avg.lw.mod + rnorm(mean = 0, sd = avg.lw.res.sd, n = nmembers),
           #precip.rate = precip.rate.mod + rnorm(mean = 0, sd = precip.rate.res.sd, n = nmembers),
           ws.mod.noise = ws.mod + rnorm(mean = 0, sd = ws.res.sd, n = 1)) %>%
    select(timestamp, dscale.member, group.num, doy, NOAA.member, temp.mod, temp.mod.noise, RH.mod, RH.mod.noise, ws.mod, ws.mod.noise)
  # group_by(timestamp) %>% # TO CHECK ENSEMBLES
  # dplyr::summarize(mod.temp = mean(temp.mod),
  #          mean.ensemble = mean(temp))
  
  
  #saveRDS(debiased, file = paste(path.working, "/debiased",sep = ""))
  return(debiased.with.noise)
}
