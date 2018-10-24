plot.title = "comparing models"
  my.formula <- y ~ x
  #my.formula2 <- y ~ x + I(x^2) + I(x^3)
joined.2 <- joined.data %>% dplyr::rename(y = ws.obs, x = ws.for) %>%
select(doy, x, y) %>%
  filter(is.na(y) == FALSE) %>%
  filter(is.na(x) == FALSE)
# think about comparing daily instead of 6 hourly
# for each day, at 20:00, adjust forecast to be same as obs (eliminating the offset)
# fix old plots (axis are off, and plot4 is not generalized (plotting wrong stuff with correct title))
# think about how to melt the two processes - we need there to be no jumps at the start of forecasting, but still take advantage of the spatial downsacaling somehow
# 


linear.model <-lm(y~x, joined.2)
summary(linear.model)$r.squared
gam.model <- mgcv::gam(y ~ s(x, k=5), data = joined.2)
summary(gam.model)$r.sq
log.model <-lm(y ~ log(x), joined.2) 
summary(log.model)$r.squared
poly.model <- lm(y ~ I(x) + I(x^2) + I(x^3), joined.2)
summary(poly.model)$r.squared



gam.model.df <- data.frame(x = joined.2$x,
                             y = fitted(gam.model))
log.model.df <- data.frame(x = joined.2$x,
                           y = fitted(log.model))
poly.model.df <- data.frame(x = joined.2$x,
                           y = fitted(exp.model))
ggplot(data = joined.2, aes(x, y), alpha = alpha) + # one plot containing all ensembles
    geom_point(alpha = 0.5*alpha) + 
    geom_smooth(aes(color = "linear"), method = "lm", se = FALSE, formula = my.formula) +
    geom_line(data = gam.model.df, aes(x, y, color = "gam")) +
    geom_line(data = log.model.df, aes(x, y, color = "log")) +
    # geom_line(data = poly.model.df, aes(x, y, color = "poly")) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE) +
    xlab("NOAA") + 
    ylab("obs") +
    ggtitle(plot.title)
slope = linear.model$coefficients[2]
b = linear.model$coefficients[1]
joined.2 <- joined.2 %>%
  mutate(mod.x = slope*x+b)
 
 ggplot(data = joined.2, alpha = alpha) + # one plot containing all ensembles
   geom_point(aes(mod.x,y, color = "linear fit"), alpha = 0.3) + 
   geom_point(aes(x,y,color = "original"), alpha = 0.3) + 
   # geom_smooth(aes(x, y, method = "lm", se = FALSE, color = "blue", formula = my.formula)) +
   xlab("NOAA") + 
   ylab("obs") +
   ggtitle("debiased and originial")
 
# original residuals 
ggplot(data = joined.2, aes()) +
  geom_point(aes(doy, y-x, color = x))
ggplot(data = joined.2, aes()) +
   geom_point(aes(x, y-x, color = doy))

# modeled residuals
ggplot(data = joined.2, aes()) +
  geom_point(aes(doy, y-mod.x, color = mod.x))
ggplot(data = joined.2, aes()) +
  geom_point(aes(mod.x, y-mod.x, color = doy))
 