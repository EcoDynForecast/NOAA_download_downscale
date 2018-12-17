add_noise <- function(debiased, coeff.df, nmembers)
debiased.with.noise <- debiased %>%
  group_by(timestamp, NOAA.member, AirTemp, RelHum, WindSpeed, ShortWave, LongWave) %>%
  expand(dscale.member = 1:nmembers) %>%
  ungroup() %>%
  group_by(dscale.member, NOAA.member) %>%
  dplyr::mutate(AirTemp = AirTemp + rnorm(mean = 0, sd = coeff.df$temp[5], n = 1),
                RelHum = RelHum + rnorm(mean = 0, sd = coeff.df$RH[5], n = 1),
                WindSpeed = WindSpeed + rnorm(mean = 0, sd = coeff.df$ws[5], n = 1),
                ShortWave = ShortWave + rnorm(mean = 0, sd = coeff.df$sw[5], n = 1),
                LongWave = LongWave + rnorm(mean = 0, sd = coeff.df$lw[5], n = 1)) %>%
  ungroup() %>%
  select(timestamp, dscale.member, NOAA.member, AirTemp, RelHum, WindSpeed, ShortWave,  LongWave)
