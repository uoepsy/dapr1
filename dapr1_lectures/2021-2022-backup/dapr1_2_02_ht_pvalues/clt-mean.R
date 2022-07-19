
setwd("~/R/uoepsy/dapr1/dapr1_lectures/dapr1_2_02_hyptest_pvalues")
rm(list = ls())
set.seed(0)

N <- 5000
x.chisq <- rexp(N, 30)
x.norm <- rnorm(N, 12, 2)

# ---------------------------------------------------

draw.hist.dens <- function(x, pop = TRUE, n = NULL, xlim = NULL) {
    if ((pop == FALSE) & is.null(n)) {
        stop("provide sample size n")
    }
    if (pop == TRUE) {
        COL = 'lightblue'
        COL.DENS = 'darkblue'
        BORDER = 'lightblue4'
        XLAB = "Data"
        MAIN = "Population data"
    } else {
        COL = 'darkolivegreen1'
        COL.DENS = 'darkorange'
        BORDER = 'darkolivegreen3'
        XLAB = "Means"
        MAIN = paste0("Means from samples of size n = ", n, " each")
    }
    hist(x, breaks = 'FD', freq = FALSE, 
         col = COL, border = BORDER, xlim = xlim,
         xlab = XLAB, main = MAIN,
         cex.lab = 1.3, cex.axis = 1.3, cex.main = 1.3)
    lines(density(x, bw = 'nrd'), 
          col = COL.DENS, lwd = 2, xlim = xlim,
          main = "",
          xlab = "")
}

get.sampl.dist <- function(x, n, n.samples = NULL) {
    if (is.null(n.samples)) n.samples = 1000
    xbars <- replicate( n.samples, mean(sample(x, size = n)) )
    return(xbars)
}

draw.clt <- function(x, n.vals = c(5, 20, 100), n.samples = NULL) {
    
    if ((length(n.vals) + 1) == 4) {
        MFROW = c(2, 2)
    } else {
        MFROW = c(length(n.vals) + 1, 1)
    }
    par(mfrow = MFROW, 
        oma = c(1,5,2,3),
        mar = c(5,0,2,2))

    draw.hist.dens(x, pop = TRUE, xlim = range(x))
    
    for (n in n.vals) {
        xbars <- get.sampl.dist(x, n, n.samples)
        draw.hist.dens(xbars, pop = FALSE, n = n, xlim = range(x))
    }
}


# ---------

png("images/2_02_hyptest_pvalues/clt-chisq.png", 
    height = 800, width = 1500, res = 100)
draw.clt(x.chisq)
dev.off()

png("images/2_02_hyptest_pvalues/clt-norm-n.png", 
    height = 800, width = 1500, res = 100)
draw.clt(x.norm)
dev.off()

png("images/2_02_hyptest_pvalues/clt-norm-1.png", 
    height = 600, width = 600)
draw.clt(x.norm, n.vals = 1, n.samples = N)
dev.off()
