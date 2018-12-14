# -----------------------------------
# Information
# -----------------------------------
# Purpose: Combine saved daily NOAA forecasts into dataframes
# Creator: Laura Puckett, December 14 2018
# Contact: plaura1@vt.edu
# -----------------------------------
# Description
# -----------------------------------
# Take the first day of each 16-day forecast and combine into single flux (longwave, shortwave, precipitation) and state (temperature, wind speed, relative humidity) dataframes
# -----------------------------------
# Input
# -----------------------------------
# @param data.path: path to SCCData-noaa-data folder, which contains 16-day NOAA forecasts (.csv) saved on many days
# -----------------------------------
process_saved_forecasts <- function(data.path){
  
  # -----------------------------------
  # 0. Load data, initialize variables
  # -----------------------------------
  forecast.files.list = list.files(data.path)
  num.ensembles = 21 # there are 21 ensemble members in GEFS forecasts
  st <- as.Date("2018-04-23") # start of available data for FCR site
  en <- as.Date(Sys.Date()) 
  flux.forecasts = NULL
  state.forecasts = NULL
  
  # -----------------------------------
  # 1. Make list of all expected dates to obtain forecast files from
  # -----------------------------------
  
  date.list <- seq(st, en, by = "1 day")
  date.list <- force_tz(as_datetime(date.list), "EST")
  
  # -----------------------------------
  # 2. Get forecast data for first day of each saved file (if data is missing for a day, print notice of missing data)
  # -----------------------------------
  
  for(i in 1:length(date.list)){
    date.path = NULL
    temp.data = NULL
    temp.year = NULL
    temp.month = NULL
    temp.day = NULL
    temp.year = lubridate::year(date.list[i])
    temp.month = lubridate::month(date.list[i])
    if(temp.month<10){
      temp.month = paste("0",temp.month, sep = "")
      }
    temp.day = lubridate::day(date.list[i])
    if(temp.day<10){
      temp.day = paste("0",temp.day, sep = "")
      }
    date.path = paste(temp.year,temp.month,temp.day, sep = "")
    
    if(paste(date.path,"gep_all_00z.csv", sep = "")%in% forecast.files.list){
      full.path = paste(data.path, date.path,"gep_all_00z.csv", sep = "")
      tmp.data = read.csv(full.path) %>% 
        mutate(forecast.date.hour = force_tz(as.POSIXct(strptime(forecast.date, "%Y-%m-%d %H:%M:%S")), "US/Eastern"),
               NOAA.file.group = i) # group number for which file the data is from
      # for states, select data up until 15th hour of day to obtain first 4 measurements (1 measurement is from previous date)
      tmp.state <- tmp.data %>%
        filter(as_datetime(forecast.date.hour) <= as_datetime(date.list[i] + 15*60*60, tz = "US/Eastern")) %>%
        select(ensembles, tmp2m, rh2m, vgrd10m, ugrd10m, forecast.date.hour, NOAA.file.group)
      # for fluxes, select data between 1st hour and 20th hour of day to obtain first 4 measurements with data (measurement from previous day is NA because flux is average over past 6 hr period and has no values until 2nd 6-hour period)   
      tmp.flux <- tmp.data %>%
        filter(as_datetime(forecast.date.hour) >= as_datetime(date.list[i] + 1*60*60, tz = "US/Eastern") &
                 as_datetime(forecast.date.hour) <= as_datetime(date.list[i] + 20*60*60, tz = "US/Eastern")) %>%
        select(ensembles, pratesfc, dlwrfsfc, dswrfsfc, forecast.date.hour, NOAA.file.group)
      
      flux.forecasts = rbind(flux.forecasts, tmp.flux)
      state.forecasts = rbind(state.forecasts, tmp.state)
    }else{
      print(paste("Missing a file for date: ", date.path, sep = ""))
    }
    
  }
  
  # -----------------------------------
  # 3. Save flux and state dataframes as Rdata
  # -----------------------------------
  
  saveRDS(flux.forecasts, file = paste(path.working,"NOAA.flux.forecasts", sep = ""))
  saveRDS(state.forecasts, file = paste(path.working,"NOAA.state.forecasts", sep = ""))
}

