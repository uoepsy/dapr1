library(tidyverse)
set.seed(0)
p = c(6, 8, 10, 12, 14)
mean(p)

n = 20
tbl = tibble('Sample' = rep(NA, n), 
             'Sample Mean' = rep(NA, n))

for (i in 1:n) {
    s = sample(p, 2)
    m = mean(s)
    s.print = paste0('(', paste(s, collapse = ', '), ')')
    m.print = round(m, 2)
    tbl[i, ] = tibble(s.print, m.print)
}
tbl

hist(tbl$`Sample Mean`)
