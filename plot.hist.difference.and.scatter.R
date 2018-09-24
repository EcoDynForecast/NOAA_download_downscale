plot.hist.difference.and.scatter <- function(df, vars.list, vars.title.list, pdf.path){
  pdf(file = pdf.path)
  my.formula <- y ~ x
  ## TEMPERATURE [C]
  for(i in 1:length(vars.list)){
    hist <- ggplot(data = joined.6.hourly) + 
      geom_density(aes(get(paste(vars.list[i],".for", sep = "")) - get(paste(vars.list[i],".obs", sep = "")), fill = "NOAA forecast - site observation"), alpha = 0.3) +
      xlab("") + 
      ggtitle(paste("Difference in ",vars.title.list[i], sep = "")) +
      theme(legend.position = "bottom")
    
    scatter <- ggplot(data = joined.6.hourly, aes(get(paste(vars.list[i],".for", sep = "")), get(paste(vars.list[i],".obs", sep = "")))) + # one plot containing all ensembles
      geom_point() + 
      geom_abline(slope = 1, intercept = 0, col = "red") +
      geom_smooth(method = "lm", se = FALSE, color = "blue", formula = my.formula) +
      stat_poly_eq(formula = my.formula, 
                   aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                   parse = TRUE) +
      xlab("forecast") + 
      ylab("obs") +
      ggtitle(vars.title.list[i])
    grid.arrange(hist, scatter, ncol = 2)
  }
  dev.off()
}
