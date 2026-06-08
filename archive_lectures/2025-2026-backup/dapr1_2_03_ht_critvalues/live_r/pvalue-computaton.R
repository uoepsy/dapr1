
n <- 50

mosaic::xpt(-1.3, df = n-1, lower.tail = F,
            return = 'plot') +
    geom_vline(xintercept = c(-1.3), 
               color = 'darkgreen', size = 2) +
    scale_fill_manual(values = c('lightgray', 'darkblue')) +
    theme_classic()

mosaic::xpt(-1.3, df = n-1,
            return = 'plot') +
    geom_vline(xintercept = c(-1.3), 
               color = 'darkgreen', size = 2) +
    scale_fill_manual(values = c('darkblue', 'lightgray')) +
    theme_classic()

mosaic::xpt(c(-1.3, 1.3), df = n-1,
            return = 'plot') +
    geom_vline(xintercept = c(-1.3, 1.3), 
               color = 'darkgreen', size = 2) +
    scale_fill_manual(values = c('darkblue', 'lightgray', 'darkblue')) +
    theme_classic()

