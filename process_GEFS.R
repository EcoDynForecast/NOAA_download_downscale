rm(list = ls())
## setup
path.working <- "/Users/laurapuckett/Documents/Research/Fall 2018/my_files/"
setwd(path.working)
DOWNSCALE = TRUE
ADD_NOISE = TRUE
# -----------------------------------
# 0. Source necessary files
# -----------------------------------

source("daily_debias_from_coeff.R")
source("spline_to_hourly.R")
source("solar_geom.R")
source("run_future_downscaling.R")
source("add_noise.R")
source("out_of_box.R")
library(imputeTS) # for out-of-box
library(stringr) # for out-of-box

# -----------------------------------
# 1. load forecast data
# -----------------------------------
in_directory = "/Users/laurapuckett/Documents/Research/Fall 2018/my_files/SCCData-noaa-data/"
out_directory = "/Users/laurapuckett/Documents/Research/Fall 2018/my_files/met_output_files"
start_date = "2018-11-18"
start_time = "2018-11-17 19:00:00 UTC"
end_time = "2018-11-18 19:00:00 UTC"
file_name = paste(year(start_time), 
                  ifelse(month(start_date)<10,
                         paste("0",month(start_date),sep = ""),
                         month(start_date)),
                  ifelse(day(start_date)<10,
                         paste("0",day(start_date),sep = ""),
                         day(start_date)),
                  "gep_all_00z", sep = "")

output_tz = "US/Eastern" 

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
  nmembers = 15 # members for downscaled ensembles
}

if(DOWNSCALE == TRUE){
  # need to add in statement here for if(CALCULATE_DEBIAS_COEFFICIENTS){}
  load(file = paste(path.working,"debiased.coefficients.RData", sep = ""))
  joined.ds = run_future_downscaling(d = d, forecast.date = forecast.date, forecast.units.match = forecast.units.match, path.working = path.working)
  ds.output = joined.ds
  
  if(ADD_NOISE == TRUE){
    joined.ds.noise = add_noise(debiased = joined.ds, coeff.df = debiased.coefficients, nmembers)
    output = joined.ds.noise
  }
  ds.output[,"timestamp"] = strftime(ds.output$timestamp, format="%Y-%m-%d %H:%M")
  ds.output = ds.output %>% plyr::rename(c("timestamp" = "full_time"))
}else{
  ## out of box
  out.of.box = out_of_box(d = d, forecast.date = forecast.date)
  out.of.box[,"timestamp"] = strftime(out.of.box$timestamp, format="%Y-%m-%d %H:%M")
  out.of.box = out.of.box %>% plyr::rename(c("timestamp" = "full_time"))
}

# format: one dataframe for each enesmble member combination
#Save in GLM Format
full_time = strftime(full_time, format="%Y-%m-%d %H:%M", tz = output_tz)
met_file_list.ds = NULL
met_file_list.no.ds = NULL

n= noquote(c('time','ShortWave','LongWave','AirTemp','RelHum','WindSpeed','Rain','Snow'))
for(NOAA.ens in 1:21){
  if(DOWNSCALE){
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




  