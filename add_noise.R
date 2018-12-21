# --------------------------------------
# purpose: create ensemble members of downscaling noise
# Creator: Laura Puckett, December 21 2018
# contact: plaura1@vt.edu
# --------------------------------------
# summary: creates ensemble members with noise addition (random sample from normal distribution with standard deviation equal to saved standard deviation of residuals from downscaling process.) For all variables except Shortwave, this is the standard deviaiton of the residuals after downscaling to the hourly resolution. For Shortwave, is is the value after downscaling to the daily resolution to artificually high values that result from noise in observational data from hour to hour.
# --------------------------------------

add_noise <- function(debiased, coeff.df, nmembers)
debiased.with.noise <- debiased %>%
  group_by(timestamp, NOAA.member, AirTemp, RelHum, WindSpeed, ShortWave, LongWave) %>%
  expand(dscale.member = 1:nmembers) %>%
  ungroup() %>%
  group_by(dscale.member, NOAA.member) %>%
  dplyr::mutate(AirTemp = AirTemp + rnorm(mean = 0, sd = coeff.df$AirTemp[5], n = 1),
                RelHum = RelHum + rnorm(mean = 0, sd = coeff.df$RelHum[5], n = 1),
                WindSpeed = WindSpeed + rnorm(mean = 0, sd = coeff.df$WindSpeed[5], n = 1),
                ShortWave = ifelse(ShortWave == 0, # not adding noise to night-time values
                                   0,
                                   ShortWave + rnorm(mean = 0, sd = coeff.df$ShortWave[3], n = 1)),
                LongWave = LongWave + rnorm(mean = 0, sd = coeff.df$LongWave[5], n = 1)) %>%
  ungroup() %>%
  select(timestamp, dscale.member, NOAA.member, AirTemp, RelHum, WindSpeed, ShortWave,  LongWave)
