process_GEFS <- function(start_date, end_time, DOWNSCALE_MET, ADD_NOISE, WRITE_FILES){
  # -----------------------------------
  # 1. read in forecast data
  # -----------------------------------
  file_name = paste(year(start_date), 
                    ifelse(month(start_date)<10,
                           paste("0",month(start_date),sep = ""),
                           month(start_date)),
                    ifelse(day(start_date)<10,
                           paste("0",day(start_date),sep = ""),
                           day(start_date)),
                    "gep_all_00z", sep = "")
  f <- paste0(in_directory,'/',file_name,'.csv')
  if(!file.exists(f)){
    print('Missing forecast file!')
    print(f)
    stop()
  }else{
    
    d <- read.csv(paste0(in_directory,'/',file_name,'.csv')) 
    d <- d %>% mutate(d = as.POSIXct(d$forecast.date, tz = output_tz))
    # d <- d %>% filter(as_datetime(forecast.date) < as_datetime(end_time) + 8*60*60)
    
    full_time <- rep(NA,length(d$forecast.date)*6)
    begin_step <- as_datetime(head(d$forecast.date,1), tz = output_tz)
    end_step <- as_datetime(tail(d$forecast.date,1), tz = output_tz)
    full_time <- seq(begin_step, end_step, by = "1 hour", tz = output_tz) # grid
  }
  
  # -----------------------------------
  # 2. adjust forecast according to desired method
  # -----------------------------------
  
  if(DOWNSCALE_MET == TRUE){
    ## Downscaling option
    print("Downscaling option")
    # need to add in statement here for if(CALCULATE_DEBIAS_COEFFICIENTS){}
    load(file = paste(path.working,"debiased.coefficients.RData", sep = ""))
    ds = run_future_downscaling(d)
    
    if(ADD_NOISE == TRUE){
      ## Downscaling + noise addition option
      print("with noise")
      ds.noise = add_noise(debiased = ds, coeff.df = debiased.coefficients, nmembers)
      output = ds.noise
    }else{
      print("without noise")
      ds = ds %>% mutate(dscale.member = 0)
      output = ds
    }
    
  }else{
    ## "out of box" option
    print("out of box option")
    out.of.box = out_of_box(d = d, forecast.date = forecast.date)
    output = out.of.box
  }
  
  # -----------------------------------
  # 4. Produce output files
  # -----------------------------------
  
  if(WRITE_FILES){
    # Rain = array(NA,dim=c(length(full_time),21))
    # Snow = array(0,dim=c(length(full_time),21))
    # for(NOAA.ens in 1:21){
    #   for(i in 1:length(full_time)){
    #     index = which(as_datetime(d$forecast.date, tz = "US/Eastern") == as_datetime(paste(full_time[i],":00", sep = ""), tz = "US/Eastern") & d$ensembles == NOAA.ens)
    #     if(length(index) > 0){
    #       if(d$pratesfc[index] < 3000){
    #         Rain[(i-6):(i-1),NOAA.ens] = d$pratesfc[index]
    #       }
    #     }
    #   }
    # }
    
    d.hrly.Rain.Snow = d %>% select(forecast.date, pratesfc, ensembles) %>%
      plyr::rename(c("forecast.date" = "full_time",
                     "pratesfc" = "Rain",
                     "ensembles" = "NOAA.member")) %>%
      dplyr::mutate(full_time = as_datetime(full_time, tz = "US/Eastern"),
                    Rain = Rain * 60*60*24*0.001,
                    Snow = NA) %>%
      group_by(Rain, Snow, NOAA.member) %>%
      expand(full_time = c(full_time,
                               full_time - 1*60*60,
                               full_time - 2*60*60,
                               full_time - 3*60*60,
                               full_time - 4*60*60,
                               full_time - 5*60*60)) %>%
      ungroup() %>%
      filter(full_time >= min(as_datetime(d$forecast.date, tz = "US/Eastern")))
      


    # format: one dataframe for each combination of ensemble members
    full_time = strftime(full_time, format="%Y-%m-%d %H:%M", tz = output_tz)
    met_file_list = NULL
    
    n= noquote(c('time','ShortWave','LongWave','AirTemp','RelHum','WindSpeed','Rain','Snow'))
    for(NOAA.ens in 1:21){
      Rain.Snow = d.hrly.Rain.Snow %>% filter(NOAA.member == NOAA.ens) %>%
        arrange(full_time, Rain, Snow) %>%
        select(full_time, Rain, Snow)
      if(DOWNSCALE_MET){
        for(dscale.ens in ifelse(ADD_NOISE, 1:nmembers, 0)){
          GLM_climate_ds = output %>%
            filter(NOAA.member == NOAA.ens & dscale.member == dscale.ens) %>%
            select(timestamp, AirTemp, WindSpeed, RelHum, ShortWave, LongWave)   
          GLM_climate_ds = GLM_climate_ds %>% plyr::rename(c("timestamp" = "full_time"))
          GLM_climate = full_join(Rain.Snow, GLM_climate_ds, by = "full_time")
          GLM_climate[,"full_time"] = strftime(GLM_climate$full_time, format="%Y-%m-%d %H:%M")
          colnames(GLM_climate) = noquote(c('time','Rain','Snow', 'AirTemp','WindSpeed', 'RelHum','ShortWave', 'LongWave'))
          current_filename = paste0(out_directory,'/','met_hourly_',file_name,'_NOAA',NOAA.ens,'_ds',dscale.ens,'.csv')
          write.csv(GLM_climate,file = current_filename, row.names = FALSE,quote = FALSE)
          met_file_list = append(met_file_list, current_filename)
        }
      }else{
        LongWave = out.of.box %>% 
          filter(NOAA.member == NOAA.ens) %>% select(LongWave)
        AirTemp = out.of.box %>% 
          filter(NOAA.member == NOAA.ens) %>% select(AirTemp)
        WindSpeed = out.of.box %>% 
          filter(NOAA.member == NOAA.ens) %>% select(WindSpeed)
        RelHum = out.of.box %>% 
          filter(NOAA.member == NOAA.ens) %>% select(RelHum)
        ShortWave = out.of.box %>% 
          filter(NOAA.member == NOAA.ens) %>% select(ShortWave)
      
        GLM_climate.partial = data.frame(full_time, AirTemp, WindSpeed, RelHum, ShortWave, LongWave)
        GLM_climate = full_join(Rain.Snow, GLM_climate.partial, by = "full_time")
        colnames(GLM_climate) = noquote(c('time','Rain','Snow', 'AirTemp','WindSpeed', 'RelHum','ShortWave', 'LongWave'))
        dscale.ens = 0
        current_filename = paste0(out_directory,'/','met_hourly_',file_name,'_NOAA',NOAA.ens,'_ds',dscale.ens,'.csv')
        write.csv(GLM_climate,file = current_filename, row.names = FALSE,quote = FALSE)
        met_file_list = append(met_file_list, current_filename)
      }
    }
  }
  return(GLM_climate)
}










