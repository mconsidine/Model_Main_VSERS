## Functions for plotting results

MATTC_get_quantiles <- function( runName,     # character
                           varName,     # character
                           data = results_all,
                           year.max = 100,
                           qts = c(0.1, 0.25, 0.5, 0.75, 0.9)){
  
  #   runName = c("R4F1")     # character
  #   varName = "FR"     # character
  #   data = results_all
  #   year.max = 100
  #   qts = c(0.1, 0.25, 0.5, 0.75, 0.9)
  #   
  
  df <- data %>% filter(runname %in% runName) %>%  
    select_("runname",  "sim","year", varName) %>% spread_("year", varName)
  
  fn <- function(df) { 
    df_q <- sapply(select(df, -sim, -runname), function(x) quantile(x, qts, na.rm = TRUE)) %>% as.data.frame
    
    df_q %<>% mutate(Quantile = rownames(df_q)) %>% gather(year, Value, -Quantile) %>%
      
      mutate(year = f2n(year),
             Quantile = factor(Quantile)) %>% filter(year <= year.max)
    
    df_q %<>% spread(Quantile, Value)
  }
  
  df <- ldply(split(df, df$runname), fn, .id = "runname")
  
  return(df)
  
}



MATTC_draw_quantiles  <- function(runName,     # character
                            varName,     # character
                            data = results_all,
                            year.max = 80,
                            qts = c(0.1, 0.25, 0.5, 0.75, 0.9),
                            ylim = NULL,
                            yscaleincr = 50){
  
  df_q <- MATTC_get_quantiles(runName = runName, 
                        varName = varName,
                        data    = data,
                        year.max = year.max,
                        qts = qts) # %>% 
    #gather(Quantile, Value, -runname, -year)
  
#  plot_q <- 
#    ggplot(df_q, aes(x = year, y = Value, color = Quantile)) + theme_bw() + 
#    geom_point(size = 1.5) + geom_line()+ 
#    labs(y = varName, title = paste0("Quantile plot of ", varName, " in ", runName))
  
#  if(length(runName) > 1) plot_q <- plot_q + facet_grid(. ~ runname) 
#  if(!is.null(ylim)) plot_q <- plot_q + coord_cartesian(ylim = ylim)
  
#  plot_q
 
vals <- df_q[,-c(1,2)]
qbreaks <- dim(vals)[2]
qhalf <- (qbreaks-1)/2
qrows <- nrow(vals)
op <- par(mar = c(5,5,4,2) + 0.1)
cols <- color.scale(1:qbreaks,extremes=c("darkgreen","grey"))
matplot(vals, type="n", xlab = 'Years', ylab = varName,
        ylim = ylim,  yaxt="n",
        main = paste0("Quantile plot of ", varName))

for (i in 1:qhalf){
  lines(vals[,i],col=cols[qbreaks-i+1]) 
  lines(vals[,qbreaks-i+1],col=cols[qbreaks-i+1])
  polygon(c(seq(1:qrows),rev(seq(1:qrows))),c(vals[,qbreaks-i+1],rev(vals[,i])),col=cols[qbreaks-i+1],border="NA")
}

abline(h=c(0.8*100),lty=2,col="red")
grid(NULL,NULL,lty=3, col="cornsilk2")
#pts <- c(pretty(as.matrix(vals), n=7),0.8*100)
pts <- c(seq(0,round(max(vals)+yscaleincr,-2),yscaleincr),80)
axis(2, at = pts, labels = paste(pts, "%", sep=""), las=2)
lines(vals[,qhalf+1],col="black")
box()
par(op)

}
