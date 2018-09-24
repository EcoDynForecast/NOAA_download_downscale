check.daily.seasonal.cycles <- function(df, vars.list, vars.title.list, pdf.path){
  pdf(file = pdf.path)
  my.formula <- y ~ x
  ## TEMPERATURE [C]
  for(i in 1:length(vars.list)){
    time.series <- ggplot(data = joined.6.hourly) +
      geom_point(aes(timestamp, get(paste(vars.list[i],".for", sep = "")), color = c("NOAA Forecast")), alpha = 0.8) +
      geom_point(aes(timestamp, get(paste(vars.list[i],".obs", sep = "")), color = c("Site Observations")), alpha = 0.8) + 
      xlab("timestamp") + 
      ylab(vars.list[i]) +
      theme(legend.position = "bottom")+ 
      ggtitle(vars.title.list[i])
                       
    yearly.diff <- ggplot(data = joined.6.hourly, aes(timestamp, get(paste(vars.list[i],".for", sep = "")) - get(paste(vars.list[i],".obs", sep = "")))) + # one plot containing all ensembles
      geom_point(alpha = 0.3) + 
      xlab("month") + 
      ylab("NOAA foreast - site observations") +
      ggtitle(vars.title.list[i])
    
    daily.scatter <- ggplot(data = joined.6.hourly) +
      geom_point(aes(lubridate::hour(timestamp), get(paste(vars.list[i],".for", sep = ""))), alpha = 0.3) +
      geom_point(aes(lubridate::hour(timestamp), get(paste(vars.list[i],".obs", sep = ""))), alpha = 0.3) + 
      xlab("hour of day") + 
      ylab(vars.list[i]) +
      theme(legend.position = "bottom")+ 
      ggtitle(vars.title.list[i])
    
    daily.diff <- ggplot(data = joined.6.hourly, aes(lubridate::hour(timestamp), get(paste(vars.list[i],".for", sep = "")) - get(paste(vars.list[i],".obs", sep = "")))) + # one plot containing all ensembles
      geom_point(alpha = 0.3) + 
      xlab("hour of day") + 
      ylab("NOAA foreast - site observations") +
      ggtitle(vars.title.list[i])
    
    grid.arrange(time.series, yearly.diff, daily.scatter, daily.diff, ncol = 2, nrow = 2)
  }
  dev.off()
}

