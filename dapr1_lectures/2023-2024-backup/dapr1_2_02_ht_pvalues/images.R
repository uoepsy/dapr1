#images.R

setwd("~/R/uoepsy/dapr1/dapr1_lectures/dapr1_2_02_hyptest_pvalues")
library(R.devices)

zz = seq(-4, 4, 0.01)
M = 5
SD = 2.1
xx = M + zz * SD

yy = dnorm(zz)
yyxx = dnorm(xx, mean = M, sd = SD)

qx = 12.2
qz = (12.2 - M) / SD

COL = c("dodgerblue")

png("images/2_02_hyptest_pvalues/x-to-z-SEs.png", 
    width = 900, height = 1000, res = 130)
    

par(mfrow = c(2, 1), mai = c(0.82, 0.82, 0.32, 0.42))
plot(xx, yyxx, type = 'l', frame.plot = F, xlim = c(min(xx), max(xx)),
     xlab = 'x: mean = 5, sd = 2.1', ylab = "", col = COL, ylim = c(0, 0.30),
     lwd = 2)
arrows(12.2, 0.10, 12.2, 0, col = 'red')
text(12.2, 0.12, labels = '12.2', col = 'red', cex = 1.5)
segments(c(-2, -1, 0, 1, 2) * SD + M, 0,
         c(-2, -1, 0, 1, 2) * SD + M, 0.20,
         col = 'darkgray', lty = 2)
text(x = c(-2, -1, 0, 1, 2) * SD + M, y = 0.23, 
     labels = c(-2, -1, 0, 1, 2) * SD + M,
     col = 'darkgray', cex = 1.5)
text(x = c(-2, -1, 0, 1, 2) * SD + M, y = 0.28, 
     labels = c('M - 2*SD', 'M - 1*SD', 'M', 'M + 1*SD', 'M + 2*SD'),
     col = 'darkgray', cex = 0.8)

plot(zz, yy, type = 'l', frame.plot = F, xlim = c(min(zz), max(zz)),
     xlab = 'z: mean = 0, sd = 1', ylab = "", col = COL, ylim = c(0, 0.6),
     lwd = 2)
arrows(qz, 0.25, qz, 0, col = 'red')
text(qz, 0.28, labels = '3.43', col = 'red', cex = 1.5)
segments(c(-2, -1, 0, 1, 2), 0,
         c(-2, -1, 0, 1, 2), 0.42,
         col = 'darkgray', lty = 2)
text(x = c(-2, -1, 0, 1, 2), y = 0.5, 
     labels = c(-2, -1, 0, 1, 2),
     col = 'darkgray', cex = 1.5)
text(x = c(-2, -1, 0, 1, 2), y = 0.6, 
     labels = c('M - 2*SD', 'M - 1*SD', 'M', 'M + 1*SD', 'M + 2*SD'),
     col = 'darkgray', cex = 0.8)
par(mfrow = c(1,1))

dev.off()

