#CONVERT GEFS DOWNLOADED FORECAST TO GLM
if (!"zoo" %in% installed.packages()) install.packages("zoo")
library(zoo)
if (!"imputeTS" %in% installed.packages()) install.packages("imputeTS")
library(imputeTS)
if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
library(lubridate)
process_GEFS2GLM <- function(in_directory,out_directory,file_name,input_tz = 'EST5EDT',output_tz = 'GMT'){
  
  file_name = "20181116gep_all_00z" # temporary just to be able to run something
  in_directory = "/Users/laurapuckett/Documents/Research/Fall 2018/my_files/SCCData-noaa-data/"
  source("solar_geom.R")
  
  f <- paste0(in_directory,'/',file_name,'.csv')
  if(!file.exists(f)){
    print('Missing forecast file!')
    print(f)
    stop()
  }else{
    d <- read.csv(paste0(in_directory,'/',file_name,'.csv')) 
    forecast.date_local <- as.POSIXct(d$forecast.date, tz = input_tz)
    d$forecast.date <- as.POSIXct(forecast.date_local, tz = output_tz)
    
    ## get forecast into names, units for downscaling process
    NOAA.na.value = 999900000000000000000
    forecast.data <- d %>%
      dplyr::mutate(timestamp = as_datetime(forecast.date, tz = "US/Eastern"),
                    timestamp = timestamp - 3600) %>% # timezone part is still wrong...temporary 
      rename(c("ensembles" = "NOAA.member"))
    
    forecast.units.match <- forecast.data %>%
      dplyr::mutate(temp = tmp2m - 273.15, # convert from K to C
                    ws = sqrt(vgrd10m^2 + ugrd10m^2),
                    avg.lw = ifelse(dlwrfsfc==NOAA.na.value, NA,dlwrfsfc),
                    avg.sw = ifelse(dswrfsfc==NOAA.na.value, NA,dswrfsfc),
                    precip.rate = ifelse( pratesfc==NOAA.na.value, NA,  pratesfc),
                    RH = rh2m) %>%
      select(NOAA.member, timestamp, temp, avg.lw, avg.sw, precip.rate, RH, ws)
    
    ## aggregate to daily
    daily_forecast = forecast.units.match %>%
      dplyr::mutate(date = date(timestamp)) %>%
      group_by(NOAA.member, date) %>%
      dplyr::summarize(temp = mean(temp, na.rm = FALSE),
                       ws = mean(ws, na.rm = FALSE),
                       RH = mean(RH, na.rm = FALSE),
                       lw = mean(avg.lw, na.rm = FALSE),
                       sw = mean(avg.sw, na.rm = FALSE),
                       precip.rate = mean(precip.rate, na.rm = FALSE)) %>%
      ungroup()
    
    # need to add in statement here for if(CALCULATE_DEBIAS_COEFFICIENTS){}
    
    ## spatial debias using saved coefficients
    if(ADD_NOISE){
      debiased <- daily_debias_from_coeff(daily_forecast, debiased.coefficients)[[2]]
    }else{
      debiased <- daily_debias_from_coeff(daily_forecast, debiased.coefficients)[[1]]
    }
    
    ## temporal downscaling of states
    NOAA.prop <- forecast.units.match %>%
      dplyr::mutate(date = date(timestamp)) %>%
      group_by(NOAA.member, date) %>%
      dplyr::mutate(temp.daily.mean = mean(temp, na.rm = FALSE),
                    RH.daily.mean = mean(RH, na.rm = FALSE),
                    ws.daily.mean = mean(ws, na.rm = FALSE)) %>%
      ungroup() %>%
      mutate(temp.prop = temp/temp.daily.mean, # proportion of daily mean that each 6-hourly measurement is
             RH.prop = RH/RH.daily.mean,
             ws.prop = ws/ws.daily.mean) %>%
      select(NOAA.member, date, timestamp, temp.prop, RH.prop, ws.prop)
    
    redistributed <- inner_join(debiased, NOAA.prop, by = c("date","NOAA.member")) %>%
      dplyr::mutate(ds.temp = temp.mod.noise * temp.prop,
                    ds.RH = RH.mod.noise * RH.prop,
                    ds.ws = ws.mod.noise * ws.prop) %>%
      ungroup() %>%
      select(NOAA.member, timestamp, dscale.member, ds.temp, ds.RH, ds.ws)
    
    splined.ds <- new_spline_NOAA_offset(redistributed) %>%
      dplyr::mutate(date = as.Date(jday, origin = as.Date("1970-01-01")),
                    hour = round((jday - as.integer(jday))*24,0),
                    timestamp = as_datetime(paste(date, " ", hour, ":","00:00", sep = ""), tz = "US/Eastern"))
    
    ## temporal downscaling of shortwave
    lat = 37.307
    lon = 360-79.837
    
    sw.hours <- debiased %>%
      dplyr::group_by(NOAA.member, date) %>%
      tidyr::expand(hour = 0:23)
    
    sw.ds <- debiased %>% 
      select(sw.mod.noise, dscale.member, NOAA.member, date) %>%
      dplyr::group_by(NOAA.member, date, dscale.member) %>%
      inner_join(sw.hours, by = c("NOAA.member","date")) %>%
      ungroup() %>%
      dplyr::mutate(timestamp = as_datetime(paste(date, " ", hour, ":","00:00", sep = ""), tz = "US/Eastern")) %>%
      dplyr::mutate(doy = yday(date) + hour/24) %>%
      dplyr::mutate(rpot = solar_geom(.$doy+4/24, lon, lat)) %>% # hourly sw flux calculated using solar geometry
      dplyr::group_by(date) %>%
      dplyr::mutate(avg.rpot = mean(rpot)) %>% # daily sw mean from solar geometry
      ungroup() %>%
      dplyr::mutate(rpot.adj = ifelse(avg.rpot > 0, sw.mod.noise * (rpot/avg.rpot),0)) # rpot.adj is the 6-houlry total from NOAA forecast (avg.sw) split into hourly 
    
    ## join state and sw output
    ds_output <- inner_join(splined.ds, sw.ds, by = c("NOAA.member","dscale.member","timestamp")) %>%
      select(timestamp, NOAA.member, dscale.member, interp.temp, interp.ws, interp.RH, rpot.adj) %>%
      rename(c("interp.temp" = "AirTemp",
             "interp.ws" = "WindSpeed",
             "interp.RH" = "RelHum",
             "rpot.adj" = "ShortWave"))
    
    # the following code chunks just convert d into desired dataframe format
    
    full_time <- rep(NA,length(d$forecast.date)*6)
    
    begin_step <- head(d$forecast.date,1)
    end_step <- tail(d$forecast.date,1)
    full_time <- seq(begin_step, end_step, by = "1 hour") # grid
    
    # ShortWave = array(NA,dim=c(length(full_time),21))
    LongWave = array(NA,dim=c(length(full_time),21))
    # AirTemp = array(NA,dim=c(length(full_time),21))
    # RelHum =array(NA,dim=c(length(full_time),21))
    # WindSpeed= array(NA,dim=c(length(full_time),21))
    Rain = array(NA,dim=c(length(full_time),21))
    Snow = array(0,dim=c(length(full_time),21))
    
    
    for(ens in 1:21){
      for(i in 1:length(full_time)){
        index = which(d$forecast.date == full_time[i] & d$ensembles == ens)
        if(length(index) > 0){
          if(d$dswrfsfc[index] < 3000){
            ShortWave[(i-6):(i-1),ens] =  d$dswrfsfc[index]
          }
          if(d$dlwrfsfc[index] < 3000){
            LongWave[(i-6):(i-1),ens] = d$dlwrfsfc[index]
          }
          if(d$tmp2m[index] < 3000){
            AirTemp[i,ens] = d$tmp2m[index]
          }
          if(d$rh2m[index] < 3000){
            RelHum[i,ens] =  d$rh2m[index]
          }
          uwind = d$ugrd10m[index]
          vwind= d$vgrd10m[index]
          if(uwind < 3000 & vwind < 3000){
            WindSpeed[i,ens] = sqrt(uwind^2 + vwind^2)
          }
          if(d$pratesfc[index] < 3000){
            Rain[(i-6):(i-1),ens] = d$pratesfc[index]
          }
        }
      }
    }
    
    for(ens in 1:21){
      #ShortWave[,ens] = na.interpolation(ShortWave[,ens], option = "spline")
      #LongWave[,ens] = na.interpolation(LongWave[,ens], option = "linear")
      AirTemp[,ens] = na.interpolation(AirTemp[,ens], option = "linear")
      RelHum[,ens] = na.interpolation(RelHum[,ens], option = "linear")
      WindSpeed[,ens] = na.interpolation(WindSpeed[,ens], option = "linear")
      #rain_na = which(is.na(Rain[,ens]))  
      #Rain[rain_na,ens] = approx(Rain[,ens],xout = rain_na,method='constant')$y
      #rain_na = which(is.na(Rain[,ens]))  
      #rain_not_na = which(!is.na(Rain[,ens]))  
      #Rain[rain_na[1]:rain_not_na[1]-1,ens] = Rain[rain_not_na[1],ens]
    }
    
    #NEED TO CONFIRM UNITS
    Rain <- Rain*60*60*24 #convert to mm/day
    Rain <- Rain*0.001
    #kg/m2/s to m/day
    
    # AirTemp <- AirTemp - 273.15 # already done within downscaling code
    
    # format: one dataframe for each enesmble member combination
    #Save in GLM Format
    full_time = strftime(full_time, format="%Y-%m-%d %H:%M", tz = output_tz)
    for(ens in 1:21){
      GLM_climate = data.frame(full_time,ShortWave[,ens],LongWave[,ens],AirTemp[,ens],RelHum[,ens],WindSpeed[,ens],Rain[,ens],Snow[,ens])
      n= noquote(c('time','ShortWave','LongWave','AirTemp','RelHum','WindSpeed','Rain','Snow'))
      colnames(GLM_climate) = noquote(c('time','ShortWave','LongWave','AirTemp','RelHum','WindSpeed','Rain','Snow'))
      write.csv(GLM_climate,file = paste0(out_directory,'/','met_hourly_',file_name,'_ens',ens,'.csv'),row.names = FALSE,quote = FALSE)
    }
    
    #### KLUDGE
    
    # full_time_day <- seq(begin_step, end_step, by = "1 day") # grid
    #  force_tz(full_time_day, tzone = "EST")
    
    # inflow = read.csv('/Users/quinn/Dropbox (VTFRS)/Research/SSC_forecasting/FCR-GLM/SCC/Forecasting/GLM/FCR_inflow_2015_022317.csv')
    #  spillway = read.csv('/Users/quinn/Dropbox (VTFRS)/Research/SSC_forecasting/FCR-GLM/SCC/Forecasting/GLM/FCR_spillway_outflow_2015_021617.csv')
    
    # inflow =  inflow[1:length(full_time_day),]
    #  inflow$time =  full_time_day
    
    # spillway = spillway[1:length(full_time_day),]
    #  spillway$time =  strftime(full_time_day, format="%Y-%m-%d")
    
    #  write.csv(inflow,file = paste(out_directory,'FCR_inflow_',file_name,'.csv',sep=''),row.names = FALSE,quote = FALSE)
    #  write.csv(spillway,file = paste(out_directory,'FCR_spillway_outflow_',file_name,'.csv',sep=''),row.names = FALSE,quote = FALSE)
  }
}
