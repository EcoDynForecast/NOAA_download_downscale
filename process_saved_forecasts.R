# to process saved NOAA dataninto one continous record
# 
# 

/Users/laurapuckett/Documents/Research/Fall 2018/SCCData-noaa-data/
  
st <- as.Date("2018-04-23")
en <- as.Date(Sys.Date())
date.list <- seq(st, en, by = "1 day")

for(i in 1:length(date.list)){
  temp.year = lubridate::year(date.list[i])
  temp.month = lubridate::month(date.list[i])
  if(temp.month<10){temp.month = paste("0",temp.month, sep = "")}
  temp.day = lubridate::day(date.list[i])
  if(temp.day<10){temp.day = paste("0",temp.day, sep = "")}
  date.path = paste(temp.year,temp.month,temp.day, sep = "")
  full.path = paste("/Users/laurapuckett/Documents/Research/Fall 2018/SCCData-noaa-data/",date.path,"gep_all_00z.csv", sep = "")
  temp.data = read.csv(full.path)
}
