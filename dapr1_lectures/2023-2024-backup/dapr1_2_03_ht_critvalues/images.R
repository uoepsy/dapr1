library(tidyverse)
library(lattice)

theme_set(theme_classic())

n = 4
tobs = 4.14

plt = tibble(
    x = seq(-7, 7, 0.01),
    y = dt(x, df = n-1)
)

tcrit = qt(c(0.025, 0.975), df = n-1)

plt.crit = plt %>%
    mutate(
        y = y * ( (x <= tcrit[1]) | (x >= tcrit[2]) )
    )

plt.pval = plt %>%
    mutate(
        y = y * ( (x <= -abs(tobs)) | (x >= abs(tobs)) )
    )

ggplot(plt) +
    geom_line(aes(x, y), color = 'blue') +
    geom_vline(xintercept = tcrit, color = 'red') + 
    geom_vline(xintercept = tobs, color = 'darkgreen') +
    geom_area(data = plt.crit,
              aes(x = x, y = y), fill = 'red', alpha = 0.5) +
    geom_area(data = plt.pval,
              aes(x = x, y = y), fill = 'darkgreen', alpha = 0.5) +
    annotate('label', x = c(-4.7, 4.7), y = c(0.3, 0.3), 
             label = c(
                 expression(frac(alpha, 2) == 0.025), 
                 expression(frac(alpha, 2) == 0.025)
             ), color = 'red', size = 6) +
    annotate('text', x = tcrit + c(0.2, -0.2), y = tcrit * 0 - 0.01,
             label = paste(c('-t* =', '+t* ='), round(tcrit, 2)),
             color = 'red', adj = c(0, 1), size = 5) +
    annotate('text', x = tobs + 0.2, y = - 0.01,
             label = paste('t =', round(tobs, 2)),
             color = 'darkgreen', adj = 0, size = 5) +
    labs(x = 't-statistics', y = 'Density')

xyplot(yy ~ xx,
       type = "l", lwd = 2, xlab = 't-statistic', ylab = 'Density',
       panel = function(x,y, ...){
           panel.xyplot(x,y, ...)
           # alpha
           xx <- c(tcrit[1], x[x<=tcrit[1]], tcrit[1]) 
           yy <- c(0,   y[x < tcrit[1]], 0) 
           panel.polygon(xx,yy, ..., col='red', 
                         border = NA, alpha = 0.75)
           xx <- c(tcrit[2], x[x >= tcrit[2]], tcrit[2]) 
           yy <- c(0,   y[x >= tcrit[2]], 0) 
           panel.polygon(xx,yy, ..., col='red', 
                         border = NA, alpha = 0.75)
           panel.abline(v = tcrit, col = 'red', lwd = 2)
           panel.text(tcrit - 0.2, -0.01, label = tcrit %>% round(2),
                      adj = 1, col = 'red', cex = 1.3)
           panel.text(c(-5.2, 5.2), 0.3, 
                      label = expression(frac(alpha, 2) == 0.025),
                      adj = 0.5, col = 'red', cex = 1.3, fontface = 'bold')
           
           # tobs
           xx <- c(-tobs, x[x<=-tobs], -tobs) 
           yy <- c(0,   y[x < -tobs], 0) 
           panel.polygon(xx,yy, ..., col='darkgreen', 
                         border = NA, alpha = 0.75)
           xx <- c(tobs, x[x >= tobs], tobs) 
           yy <- c(0,   y[x >= tobs], 0) 
           panel.polygon(xx,yy, ..., col='darkgreen', 
                         border = NA, alpha = 0.75)
           panel.abline(v = tobs, col = 'darkgreen', lwd = 2)
           panel.text(tobs + 0.2, -0.01, label = tobs %>% round(2),
                      adj = 0, col = 'darkgreen', cex = 1.3)
       })
