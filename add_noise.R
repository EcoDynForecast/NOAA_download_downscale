add_noise <- function(debiased, coeff.df, nmembers)
debiased.with.noise <- debiased  %>% select(-dscale.member) %>%
  group_by(date, NOAA.member, temp.mod, RH.mod,  ws.mod, sw.mod, lw.mod) %>%
  expand(dscale.member = 1:nmembers) %>%
  ungroup() %>%
  group_by(date, dscale.member, NOAA.member) %>%
  dplyr::mutate(temp.mod= temp.mod + rnorm(mean = 0, sd = coeff.df$temp[6], n = 1),
                RH.mod = RH.mod + rnorm(mean = 0, sd = coeff.df$RH[6], n = 1),
                ws.mod = ws.mod + rnorm(mean = 0, sd = coeff.df$ws[6], n = 1),
                sw.mod = sw.mod + rnorm(mean = 0, sd = coeff.df$sw[6], n = 1),
                lw.mod = lw.mod + rnorm(mean = 0, sd = coeff.df$lw[6], n = 1)) %>%
  ungroup() %>%
  select(date, dscale.member, NOAA.member, temp.mod, RH.mod, ws.mod, sw.mod,  lw.mod)
