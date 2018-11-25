daily_debias_coeff <- function(joined.data, nmembers){
  
  get_lm_coeff <- function(col.obs, col.for){
    model = lm(col.obs ~ col.for)
    slope = model$coefficients[2]
    intercept = model$coefficients[1]
    return(list(slope, intercept))
  }
  
  lm.res.sd <- function(col.obs, col.for){
    model = lm(col.obs ~ col.for)
    res.sd = sd(residuals(model))
    return(res.sd)
  }
  df = data_frame(temp = rep(NA,3), RH = rep(NA,3), ws = rep(NA,3), sw = rep(NA,3))
  # slope
  df$temp[1] = get_lm_coeff(joined.data$temp.obs, joined.data$temp.for)[[1]]
  df$RH[1] = get_lm_coeff(joined.data$RH.obs, joined.data$RH.for)[[1]]
  df$ws[1] = get_lm_coeff(joined.data$ws.obs, joined.data$ws.for)[[1]]
  df$sw[1] = get_lm_coeff(joined.data$sw.obs, joined.data$sw.for)[[1]]
  # intercept
  df$temp[2] = get_lm_coeff(joined.data$temp.obs, joined.data$temp.for)[[2]]
  df$RH[2] = get_lm_coeff(joined.data$RH.obs, joined.data$RH.for)[[2]]
  df$ws[2] = get_lm_coeff(joined.data$ws.obs, joined.data$ws.for)[[2]]
  df$sw[2] = get_lm_coeff(joined.data$sw.obs, joined.data$sw.for)[[2]]
  # standard deviation of residuals
  df$temp[3] = lm.res.sd(joined.data$temp.obs, joined.data$temp.for)
  df$RH[3] = lm.res.sd(joined.data$RH.obs, joined.data$RH.for)
  df$ws[3] = lm.res.sd(joined.data$ws.obs, joined.data$ws.for)
  df$sw[3] = lm.res.sd(joined.data$sw.obs, joined.data$sw.for)

  return(df)
}
