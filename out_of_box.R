out_of_box <- function(d, forecast.date){
  d.6hr = d %>% select(forecast.date, ensembles, pratesfc, dswrfsfc, dlwrfsfc, tmp2m, rh2m, ugrd10m, vgrd10m) %>%
    plyr::rename(c("forecast.date" = "full_time",
                   "ensembles" = "NOAA.member",
                   "pratesfc" = "Rain",
                   "dswrfsfc" = "ShortWave",
                   "dlwrfsfc" = "LongWave",
                   "tmp2m" = "AirTemp",
                   "rh2m" = "RelHum",
                   "ugrd10m" = "uWind",
                   "vgrd10m" = "vWind")) %>%
    dplyr::mutate(full_time = as_datetime(full_time, tz = "US/Eastern"),
                  Rain = Rain * 60*60*24*0.001,
                  Snow = NA,
                  WindSpeed = sqrt(uWind^2 + vWind^2)) %>%
    select(-uWind, -vWind)
  d.hrly <- d.6hr %>%
    group_by(NOAA.member, Rain, Snow, ShortWave, LongWave) %>%
    expand(full_time = c(full_time,
                         full_time - 1*60*60,
                         full_time - 2*60*60,
                         full_time - 3*60*60,
                         full_time - 4*60*60,
                         full_time - 5*60*60)) %>%
    ungroup() %>%
    filter(full_time >= min(as_datetime(d$forecast.date, tz = "US/Eastern"))) %>% 
    full_join(d.6hr %>% select(-Rain, -Snow, -ShortWave, -LongWave), by = c("full_time","NOAA.member")) %>%
    dplyr::arrange(NOAA.member, full_time) %>%
    dplyr::group_by(NOAA.member) %>%
    dplyr::mutate(AirTemp = na.interpolation(AirTemp, option = "linear"),
           RelHum = na.interpolation(RelHum, option = "linear"),
           WindSpeed = na.interpolation(WindSpeed, option = "linear")) %>%
    ungroup()
  
  
  
  
  
  print("DOWNSCALE_MET = FALSE")
  ## This section is Quinn's Code using "out of box" version
  ShortWave = array(NA,dim=c(length(full_time),21))
  LongWave = array(NA,dim=c(length(full_time),21))
  AirTemp = array(NA,dim=c(length(full_time),21))
  RelHum =array(NA,dim=c(length(full_time),21))
  WindSpeed= array(NA,dim=c(length(full_time),21))
  
  
  
  
  
  
  
  for(NOAA.ens in 1:21){
    for(i in 1:length(full_time)){
      index = which(as_datetime(d$forecast.date, tz = "US/Eastern") == as_datetime(full_time[i], tz = "US/Eastern") & d$ensembles == NOAA.ens)
      if(length(index) > 0){
        if(d$dswrfsfc[index] < 3000){
          ShortWave[(i-6):(i-1),NOAA.ens] =  d$dswrfsfc[index]
        }
        if(d$dlwrfsfc[index] < 3000){
          LongWave[(i-6):(i-1),NOAA.ens] = d$dlwrfsfc[index]
        }
        if(d$tmp2m[index] < 3000){
          AirTemp[i,NOAA.ens] = d$tmp2m[index]
        }
        if(d$rh2m[index] < 3000){
          RelHum[i,NOAA.ens] =  d$rh2m[index]
        }
        uwind = d$ugrd10m[index]
        vwind= d$vgrd10m[index]
        if(uwind < 3000 & vwind < 3000){
          WindSpeed[i,NOAA.ens] = sqrt(uwind^2 + vwind^2)
        }
      }
    }
  }
  for(NOAA.ens in 1:21){
    #ShortWave[,NOAA.ens] = na.interpolation(ShortWave[,NOAA.ens], option = "spline")
    LongWave[,NOAA.ens] = na.interpolation(LongWave[,NOAA.ens], option = "linear")
    AirTemp[,NOAA.ens] = na.interpolation(AirTemp[,NOAA.ens], option = "linear")
    RelHum[,NOAA.ens] = na.interpolation(RelHum[,NOAA.ens], option = "linear")
    WindSpeed[,NOAA.ens] = na.interpolation(WindSpeed[,NOAA.ens], option = "linear")
  }
  ## formatting for evaluate_downscaling function, this is not actually part of downscaling
  full_time.df = as.data.frame(full_time)
  LongWave.df = as.data.frame(LongWave) %>% cbind(full_time.df) %>%
    gather(NOAA.member, LongWave, V1:V21)
  AirTemp.df = as.data.frame(AirTemp) %>% cbind(full_time.df) %>%
    gather(NOAA.member, AirTemp, V1:V21)
  WindSpeed.df = as.data.frame(WindSpeed)  %>% cbind(full_time.df) %>%
    gather(NOAA.member, WindSpeed, V1:V21)
  RelHum.df = as.data.frame(RelHum)  %>% cbind(full_time.df) %>%
    gather(NOAA.member, RelHum, V1:V21)
  ShortWave.df = as.data.frame(ShortWave) %>% cbind(full_time.df) %>%
    gather(NOAA.member, ShortWave, V1:V21)
  
  out.of.box = LongWave.df %>%
    inner_join(AirTemp.df, by = c("full_time","NOAA.member")) %>%
    inner_join(WindSpeed.df, by = c("full_time","NOAA.member")) %>%
    inner_join(RelHum.df, by = c("full_time","NOAA.member")) %>%
    inner_join(ShortWave.df, by = c("full_time","NOAA.member")) %>%
    dplyr::mutate(NOAA.member = as.integer(str_replace(NOAA.member, "V",""))) %>%
    plyr::rename(c("full_time" = "timestamp")) %>%
    dplyr::mutate(timestamp = as_datetime(timestamp))
  save(out_of_box, file = '/Users/laurapuckett/Documents/Research/Fall 2018/my_files/out_of_box.RData')
  return(out.of.box) 
}

