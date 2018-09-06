# to process saved NOAA dataninto one continous record
# should change to get files directly from github
# need to split by ensemble
# make column to say which date the data is coming from
# clean up and comment
# 


st <- as.Date("2018-04-23")
en <- as.Date(Sys.Date())
date.list <- seq(st, en, by = "1 day")
forecasts_combined = NULL
forecast.files.list = list.files(path = "/Users/laurapuckett/Documents/Research/Fall 2018/SCCData-noaa-data/")
for(i in 1:length(date.list)){
  temp.year = lubridate::year(date.list[i])
  temp.month = lubridate::month(date.list[i])
  if(temp.month<10){temp.month = paste("0",temp.month, sep = "")}
  temp.day = lubridate::day(date.list[i])
  if(temp.day<10){temp.day = paste("0",temp.day, sep = "")}
  
  date.path = paste(temp.year,temp.month,temp.day, sep = "")
  
  if(paste(date.path,"gep_all_00z.csv", sep = "")%in% forecast.files.list){
    full.path = paste("/Users/laurapuckett/Documents/Research/Fall 2018/SCCData-noaa-data/",date.path,"gep_all_00z.csv", sep = "")
    last.data = date.list[i]
    }else if(paste(date.path,"gep_all_06z.csv", sep = "")%in% forecast.files.list){
      full.path = paste("/Users/laurapuckett/Documents/Research/Fall 2018/SCCData-noaa-data/",date.path,"gep_all_06z.csv", sep = "")
      last.data = date.list[i]
      last.path = full.path
    }else if(paste(date.path,"gep_all_12z.csv", sep = "")%in% forecast.files.list){
      full.path = paste("/Users/laurapuckett/Documents/Research/Fall 2018/SCCData-noaa-data/",date.path,"gep_all_12z.csv", sep = "")
      last.data = date.list[i]
      last.path = full.path
    }else if(paste(date.path,"gep_all_18z.csv", sep = "")%in% forecast.files.list){
      full.path = paste("/Users/laurapuckett/Documents/Research/Fall 2018/SCCData-noaa-data/",date.path,"gep_all_18z.csv", sep = "")
      last.data = date.list[i]
      last.path = full.path
    }else{
      print(paste("You are missing a file for date: ", date.path, sep = ""))
      diff.day = date.list[i] - last.data
      full.path = last.path
      }
    
  temp.data = read.csv(full.path) %>% 
    mutate(forecast.date = lubridate::as_date(forecast.date)) %>%
    filter(forecast.date == date.list[i])
  forecasts_combined = rbind(forecasts_combined, temp.data)
  temp.year = NULL
  temp.month = NULL
  temp.day = NULL
  temp.data = NULL
  full.path = NULL
}
