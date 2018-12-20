process_GEFS <- function(start_date, end_time, DOWNSCALE_MET, ADD_NOISE, WRITE_FILES){
  # -----------------------------------
  # 1. read in and reformat forecast data
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
    full_time <- rep(NA,length(d$forecast.date)*6)
    begin_step <- as_datetime(head(d$forecast.date,1), tz = output_tz)
    end_step <- as_datetime(tail(d$forecast.date,1), tz = output_tz)
    full_time <- seq(begin_step, end_step, by = "1 hour", tz = output_tz) # grid
    forecasts <- prep_for(d)

    forecasts[which(forecasts$timestamp == min(forecasts$timestamp)),]$ShortWave = forecasts[which(forecasts$timestamp == min(forecasts$timestamp) + 24*60*60),]$ShortWave
    # hack to give sw values for 1st measurement (that are in fact the values for the second day). This is to avoid having NAs for the first few hours of forecast
    forecasts[which(forecasts$timestamp == min(forecasts$timestamp)),]$LongWave = forecasts[which(forecasts$timestamp == min(forecasts$timestamp) + 6*60*60),]$LongWave
    # hack to give lw values for 1st measurement (that are in fact the values of the next measurement, 6 hours later). This is to avoid having NAs for the first few hours of forecast
    
  }
  
  # -----------------------------------
  # 2. adjust forecast according to desired method
  # -----------------------------------
  ## create a function here? that applies bounds to forecasted data
  if(DOWNSCALE_MET == TRUE){
    ## Downscaling option
    print("Downscaling option")
    # need to add in statement here for if(CALCULATE_DEBIAS_COEFFICIENTS){}
    load(file = paste(path.working,"debiased.coefficients.RData", sep = ""))
    ds = downscale_met(forecasts,VarNames, VarNamesStates, FALSE, FALSE, FALSE)
    
    if(ADD_NOISE == TRUE){
      ## Downscaling + noise addition option
      print("with noise")
      ds.noise = add_noise(debiased = ds, coeff.df = debiased.coefficients, nmembers)
    }else{
      print("without noise")
      ds = ds %>% mutate(dscale.member = 0)
    }
    
  }else{
    ## "out of box" option
    print("out of box option")
    out.of.box = out_of_box(d = forecasts, forecast.date = forecast.date)
  }
  
  # -----------------------------------
  # 3. Produce output files
  # -----------------------------------
  
  if(WRITE_FILES){
    # Rain and Snow are currently not downscaled, so they are calculated here
    hrly.Rain.Snow = forecasts %>% select(timestamp, Rain) %>%
                    dplyr::mutate(Snow = NA) %>%
      group_by(Rain, Snow, NOAA.member) %>%
      expand(full_time = c(full_time,
                           full_time - 1*60*60,
                           full_time - 2*60*60,
                           full_time - 3*60*60,
                           full_time - 4*60*60,
                           full_time - 5*60*60)) %>%
      ungroup() %>%
      filter(timestamp >= min(as_datetime(d$forecast.date, tz = "US/Eastern"))) %>%
      arrange(timestamp)
    
    write_file <- function(){
      GLM_climate[,"full_time"] = strftime(GLM_climate$full_time, format="%Y-%m-%d %H:%M")
      colnames(GLM_climate) = noquote(c('time','Rain','Snow', 'AirTemp','WindSpeed', 'RelHum','ShortWave', 'LongWave'))
      current_filename = paste0(out_directory,'/','met_hourly_',file_name,'_NOAA',NOAA.ens,'_ds',dscale.ens,'.csv')
      write.csv(GLM_climate,file = current_filename, row.names = FALSE, quote = FALSE)
      return(current_filename)
    }
    met_file_list = NULL
    
    for(NOAA.ens in 1:21){
      Rain.Snow = hrly.Rain.Snow %>% filter(NOAA.member == NOAA.ens) %>%
        select(full_time, Rain, Snow)
      if(DOWNSCALE_MET){
        if(ADD_NOISE){ # downscale met with noise addition
          for(dscale.ens in 1:nmembers){
            GLM_climate_ds = ds.noise %>%
              filter(NOAA.member == NOAA.ens & dscale.member == dscale.ens) %>%
              select(timestamp, AirTemp, WindSpeed, RelHum, ShortWave, LongWave)
            GLM_climate_ds = GLM_climate_ds %>% plyr::rename(c("timestamp" = "full_time"))
            GLM_climate = full_join(Rain.Snow, GLM_climate_ds, by = "full_time")
            current_filename = write_file()
            met_file_list = append(met_file_list, current_filename)
          }
        }else{ # downscale met, no noise addition
          GLM_climate_ds = ds %>%
            filter(NOAA.member == NOAA.ens) %>%
            select(timestamp, AirTemp, WindSpeed, RelHum, ShortWave, LongWave)
          GLM_climate_ds = GLM_climate_ds %>% plyr::rename(c("timestamp" = "full_time"))
          GLM_climate = full_join(Rain.Snow, GLM_climate_ds, by = "full_time")
          current_filename = write_file()
          met_file_list = append(met_file_list, current_filename)
        }
      }else{ # out of box
        GLM_climate = out.of.box %>% plyr::rename(c("timestamp" = "full_time")) %>%
          filter(NOAA.member == NOAA.ens) %>%
          arrange(full_time) %>%
          select(full_time, Rain, Snow, AirTemp, WindSpeed, RelHum, ShortWave, LongWave)
        current_filename = write_file()
        met_file_list = append(met_file_list, current_filename)
      }
    }
  }
  return(met_file_list)
}










