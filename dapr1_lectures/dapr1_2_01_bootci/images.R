library(tidyverse)
library(patchwork)

setwd("~/R/uoepsy/dapr1/dapr1_lectures/dapr1_2_01_bootci")

set.seed(321)

opts = list(W = 5, H = 6)

data1 = rt(500, 10)
data2 = rt(500, 10)

p1 = qplot(data1, geom = 'dotplot', dotsize = 0.5, stackratio = 1.5, 
                 color = I(NA), fill = I('darkolivegreen4')) +
    labs(y = "", x = expr(bar(x)), title = 'Sampling distribution') +
    theme_minimal(base_size = 16) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    xlim(-3, 3)
p1

ggsave('2_01_sampl_distrib.png', width = opts$W, height = opts$H)

p2 = qplot(data2, geom = 'dotplot', dotsize = 0.5, stackratio = 1.5, 
      color = I(NA), fill = I('darkorange')) +
    labs(y = "", x = expr(bar(x)), title = 'Bootstrap distribution') +
    theme_minimal(base_size = 16) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    xlim(-3, 3)
p2

ggsave('2_01_boot_distrib.png', width = opts$W, height = opts$H)


p2 | p1

ggsave('2_01_bootsampl_distrib.png', width = opts$W * 2, height = opts$H)


p3 = ggplot(tibble(data = data2)) + 
    geom_dotplot(aes(x = data), dotsize = 0.5, stackratio = 1.5, 
                 color = NA, fill = 'darkorange') +
    geom_function(fun = \(x) 2.0*dt(x, 10), colour = "blue", size = 1.5) +
    theme_minimal(base_size = 16) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(y = "", x = expr(bar(x)))  +
    xlim(-3, 3)
p3

ggsave('2_01_simul_theory.png', width = opts$W, height = opts$H)


p4 = ggplot(tibble(data = data2)) + 
    geom_dotplot(aes(x = data), dotsize = 0.5, 
                 color = NA, fill = 'darkorange', method = 'histodot') +
    theme_minimal(base_size = 16) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(y = "", x = expr(bar(x)))  +
    xlim(-3, 3) +
    ylim(0, 1)
p4

p5 = ggplot(tibble(data = data2)) + 
    geom_function(fun = \(x) 2 * dt(x, 10), colour = "blue", size = 1.5) +
    theme_minimal(base_size = 16) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(y = "", x = expr(bar(x)))  +
    xlim(-3, 3) +
    ylim(0, 1)
p5

p4 | p5
ggsave('2_01_se_comp.png', width = opts$W * 2, height = opts$H)