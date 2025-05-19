setwd("_data/")

library(tidyverse)
df = read_csv("dataset-ipeds-2012-subset2.csv")
head(df)

ggplot(df, aes(x = gradratew)) +
    geom_histogram(color = 'white')

ggplot(df, aes(x = gradratew)) +
    geom_density()

df %>%
    summarise(n = length(gradratew),
              M = mean(gradratew),
              SD = sd(gradratew))

psych::describe(df$gradratew)

ggplot(df, aes(sample = gradratew)) +
    geom_qq() +
    geom_qq_line()

shapiro.test(df$gradratew)

source('https://uoepsy.github.io/files/rep_sample_n.R')

tmp = df %>%
    rep_sample_n(n = nrow(df), samples = 1000, replace = TRUE)

tmp.2 = tmp %>%
    group_by(sample) %>%
    summarise(M = mean(gradratew))

ggplot(tmp.2, aes(sample = M)) +
    geom_qq() +
    geom_qq_line()

shapiro.test(tmp.2$M)


xbar = mean(df$gradratew)
n = nrow(df)
tstar = qt(c(0.025, 0.975), n - 1)
se = sd(df$gradratew) / sqrt(n)

xbar + tstar * se

t.test(df$gradratew, mu = 50)

xbar

(xbar - 50) / sd(df$gradratew)
