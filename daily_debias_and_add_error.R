daily_debias_and_add_error <- function(joined.data, nmembers){

  # End Goal: plot NOAA ensembles vs downscaled NOAA ensemble members
  

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
  ws.res.sd = lm.res.sd(joined.data$ws.obs, joined.data$ws.for)
  
  debiased <- joined.data %>% 
    dplyr::mutate(temp.mod =  lin.mod(joined.data$temp.obs, joined.data$temp.for),
           RH.mod =  lin.mod(joined.data$RH.obs, joined.data$RH.for),
           ws.mod =  lin.mod(joined.data$ws.obs, joined.data$ws.for)) %>% 
    select(group.num, doy, NOAA.member, temp.mod, RH.mod, ws.mod)
  
# 
#   debiased.with.noise <- debiased %>%
#     group_by(timestamp, group.num, doy, NOAA.member, temp.mod, RH.mod,  ws.mod) %>%
#     expand(dscale.member = 1:nmembers) %>%
#     ungroup() %>%
#     group_by(timestamp, dscale.member, group.num, doy, NOAA.member, temp.mod, RH.mod,  ws.mod) %>%
#     dplyr::mutate(temp.mod.noise = temp.mod + rnorm(mean = 0, sd = temp.res.sd, n = 1),
#            RH.mod.noise = RH.mod + rnorm(mean = 0, sd = RH.res.sd, n = 1),
#            ws.mod.noise = ws.mod + rnorm(mean = 0, sd = ws.res.sd, n = 1)) %>%
#     select(timestamp, dscale.member, group.num, doy, NOAA.member, temp.mod, temp.mod.noise, RH.mod, RH.mod.noise, ws.mod, ws.mod.noise)
  # group_by(timestamp) %>% # TO CHECK ENSEMBLES
  # dplyr::summarize(mod.temp = mean(temp.mod),
  #          mean.ensemble = mean(temp))
  
  
  #saveRDS(debiased, file = paste(path.working, "/debiased",sep = ""))
  return(debiased)
}
