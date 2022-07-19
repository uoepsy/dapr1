library(tidyverse)

n.reps = 10000
n = 10

t.h0 = replicate(n.reps, {
    x = rnorm(n, 0, 1)
    xbar = mean(x)
    s = sd(x)
    tobs = (xbar - 0) / (s / sqrt(n))
})

t.h1 = replicate(n.reps, {
    x = rnorm(n, 2, 1)
    xbar = mean(x)
    s = sd(x)
    tobs = (xbar - 0) / (s / sqrt(n))
})

plt = tibble(
    t.h0 = t.h0,
    t.h1 = t.h1
)

plt.long = plt %>%
    pivot_longer(1:2, names_to = 'true.hyp', values_to = 't.stat') %>%
    mutate(true.hyp.lab = ifelse(true.hyp == 't.h0', 'H0 true', 'H1 true'))


ggplot(plt.long) +
    geom_density(aes(t.stat, color = true.hyp.lab), bw = 1) +
    facet_grid(true.hyp.lab ~ .) +
    theme_bw(base_size = 15) + 
    theme(legend.position = 'none') +
    geom_vline(xintercept = 2, color = 'red')
