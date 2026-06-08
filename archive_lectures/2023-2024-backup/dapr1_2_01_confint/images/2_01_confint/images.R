library(tidyverse)
library(patchwork)

setwd("~/R/uoepsy/dapr1/dapr1_lectures/dapr1_2_01_bootci")

set.seed(456)

opts = list(W = 5, H = 6)

data1 = rt(500, 10)
data2 = rt(500, 10) / 2

p1 = qplot(data1, geom = 'dotplot', dotsize = 0.5, stackratio = 1.5, 
                 color = I(NA), fill = I('darkolivegreen4')) +
    labs(y = "", x = expr(bar(x)), title = 'Sampling distribution') +
    theme_minimal(base_size = 16) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    xlim(-3, 3)
p1

ggsave('sampl_distrib.png', width = opts$W, height = opts$H)

p2 = ggplot(tibble(data = data1)) + 
    geom_dotplot(aes(x = data), dotsize = 0.5, stackratio = 1.5, 
           color = NA, fill = 'darkolivegreen4') +
    geom_function(fun = \(x) 2.2*dt(x, 10), colour = "blue", size = 1.5) +
    labs(y = "", x = expr(bar(x)), title = 'Sampling distribution') +
    theme_minimal(base_size = 16) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    xlim(-3, 3)
p2

ggsave('sampl_t.png', width = opts$W, height = opts$H)


p3 = ggplot(tibble(data = data2)) + 
    geom_dotplot(aes(x = data), dotsize = 0.4, stackratio = 1.1, 
                 color = NA, fill = 'darkolivegreen4') +
    geom_function(fun = \(x) 2.5*dt(x*2, 10), colour = "blue", size = 1.5) +
    labs(y = "", x = expr(bar(x)), title = 'Sampling distribution') +
    theme_minimal(base_size = 16) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    xlim(-3, 3)
p3

ggsave('sampl_t_2.png', width = opts$W, height = opts$H)
