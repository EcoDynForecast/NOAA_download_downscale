get_daily_debias_coeff <- function(joined.data){
  # --------------------------------------
  # purpose: save coefficients for linear debiasing (slope, intercept, standard deviation of residuals, r2 of linear regression)
  # Creator: Laura Puckett, December 14 2018
  # --------------------------------------
  
  get_lm_coeff <- function(col.obs, col.for){
    model = lm(col.obs ~ col.for)
    intercept = model$coefficients[1]
    slope = model$coefficients[2]
    res.sd = sd(residuals(model))
    r2 = summary(model)$r.squared
    return(list(intercept, slope, res.sd, r2))
  }
  
  df = data_frame(temp = rep(NA,6), RH = rep(NA,6), ws = rep(NA,6), sw = rep(NA,6), lw = rep(NA,6))
  
  for (i in 1:4){
    df$temp[i] = get_lm_coeff(joined.data$temp.obs, joined.data$temp.for)[[i]]
    df$RH[i] = get_lm_coeff(joined.data$RH.obs, joined.data$RH.for)[[i]]
    df$ws[i] = get_lm_coeff(joined.data$ws.obs, joined.data$ws.for)[[i]]
    df$sw[i] = get_lm_coeff(joined.data$sw.obs, joined.data$sw.for)[[i]]
    df$lw[i] = get_lm_coeff(joined.data$lw.obs, joined.data$lw.for)[[i]]
  }
  df = as.data.frame(df) 
  row.names(df) <- c("intercept", "slope", "sd.res.daily", "r2.daily", "ds.res.hourly", "r2.hourly")
  # could convert to key column later instead of row,column

  return(df)
}
