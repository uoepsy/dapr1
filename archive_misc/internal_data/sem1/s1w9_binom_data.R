# https://today.yougov.com/topics/society/articles-reports/2022/09/29/how-do-people-us-take-their-coffee
# P(coffee) = 3/4 = 0.75

set.seed(5678)

library(tidyverse)  # we use read_csv and glimpse from tidyverse
tips <- read_csv("https://uoepsy.github.io/data/RestaurantTips.csv")
head(tips)

table(tips$Guests)

# keep only parties of 2 people
tips2 <- tips %>%
    filter(Guests == 2)

N = nrow(tips2)

# Binomial data
tips2$HadCoffee <- rbinom(n = N, size = 2, prob = 0.75)

phat = mean(tips2$HadCoffee) / 2
phat
0.75 - phat

# Normal data
tips2$IQ1 <- round(rnorm(N, 100, 15), 0)
tips2$IQ2 <- round(rnorm(N, 100, 15), 0)

write_csv(tips2, "RestaurantTips2.csv")
write_csv(tips2, "../../data/RestaurantTips2.csv")
