daily_to_6hr <- function(forecasts, daily.forecast, debiased){
  forecasts <- forecasts %>%
    dplyr::mutate(date = date(timestamp))
  deviations <- full_join(daily.forecast, forecasts, by = c("date","NOAA.member"), suffix = c(".daily",".6hr"))
  devNames = NULL
  for(Var in 1:length(VarNames)){
    deviations[,paste0(VarNames[Var],".dev")] = deviations[,paste0(VarNames[Var], ".6hr")] - deviations[,paste0(VarNames[Var], ".daily")]
    devNames = append(devNames, paste0(VarNames[Var],".dev"))
  }
  deviations <- deviations %>% select(date, timestamp, NOAA.member, devNames)
  
  redistributed <- inner_join(debiased, deviations, by = c("date","NOAA.member"))
  for(Var in 1:length(VarNames)){
    redistributed[,VarNames[Var]] = redistributed[,VarNames[Var]] + redistributed[,paste0(VarNames[Var], ".dev")]
  }
  
  redistributed <- redistributed %>% select(NOAA.member, timestamp, VarNames)
  return(redistributed)
}

