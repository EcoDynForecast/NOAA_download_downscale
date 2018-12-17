

process_GEFS <- function(start_date, end_time, DOWNSCALE_MET, ADD_NOISE){
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
    d <- d %>% filter(as_datetime(forecast.date) < as_datetime(end_time) + 8*60*60)
    
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
      ds.noise = add_noise(debiased = joined.ds, coeff.df = debiased.coefficients, nmembers)
      output = ds.noise
    }else{
      print("without noise")
      ds = ds %>% mutate(dscale.member = 0)
      output = ds
    }
    
  }else{
    ## "out of box" option
    ("out of box option")
    out.of.box = out_of_box(d = d, forecast.date = forecast.date)
    output = out.of.box
  }
  output.for.return = output
  output[,"timestamp"] = strftime(output$timestamp, format="%Y-%m-%d %H:%M")
  output = output %>% plyr::rename(c("timestamp" = "full_time"))
  
  # -----------------------------------
  # 4. Produce output files
  # -----------------------------------
  
  if(WRTIE_FILES){
    # format: one dataframe for each combination of ensemble members
    full_time = strftime(full_time, format="%Y-%m-%d %H:%M", tz = output_tz)
    met_file_list.ds = NULL
    met_file_list.no.ds = NULL
    
    n= noquote(c('time','ShortWave','LongWave','AirTemp','RelHum','WindSpeed','Rain','Snow'))
    for(NOAA.ens in 1:21){
      if(DOWNSCALE_MET){
        for(dscale.ens in 1:nmembers){
          GLM_climate_no_ds = data.frame(full_time,Rain[,NOAA.ens],Snow[,NOAA.ens]) %>%
            dplyr::mutate(full_time = as.character(full_time))
          GLM_climate_ds = output %>%
            filter(NOAA.member == NOAA.ens & dscale.member == dscale.ens) %>%
            select(full_time, AirTemp, WindSpeed, RelHum, ShortWave, LongWave)
          GLM_climate = full_join(GLM_climate_no_ds, GLM_climate_ds, by = "full_time")
          colnames(GLM_climate) = noquote(c('time','Rain','Snow', 'AirTemp','WindSpeed', 'RelHum','ShortWave', 'LongWave'))
          current_filename = paste0(out_directory,'/','met_hourly_',file_name,'_NOAA',NOAA.ens,'_ds',dscale.ens,'.csv')
          write.csv(GLM_climate,file = current_filename, row.names = FALSE,quote = FALSE)
          met_file_list.ds = append(met_file_list.ds, current_filename)
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
        
        GLM_climate = data.frame(full_time, LongWave, Rain[,NOAA.ens], Snow[,NOAA.ens], AirTemp, WindSpeed, RelHum, ShortWave)
        colnames(GLM_climate) = noquote(c('time','LongWave','Rain','Snow', 'AirTemp','WindSpeed', 'RelHum','ShortWave'))
        dscale.ens = 0
        current_filename = paste0(out_directory,'/','met_hourly_',file_name,'_NOAA',NOAA.ens,'_ds',dscale.ens,'.csv')
        write.csv(GLM_climate,file = current_filename, row.names = FALSE,quote = FALSE)
        met_file_list.no.ds = append(met_file_list.no.ds, current_filename)
      }
    }
  }
  return(output.for.return)
}








