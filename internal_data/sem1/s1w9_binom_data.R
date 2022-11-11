# https://today.yougov.com/topics/society/articles-reports/2022/09/29/how-do-people-us-take-their-coffee
# P(coffee) = 3/4 = 0.75

set.seed(1234)

library(tidyverse)  # we use read_csv and glimpse from tidyverse
tips <- read_csv("https://uoepsy.github.io/data/RestaurantTips.csv")
head(tips)

table(tips$Guests)

# keep only parties of 2 people
tips2 <- tips %>%
    filter(Guests == 2)

N = nrow(tips2)
tips2$HadCoffee <- rbinom(n = N, size = 2, prob = 0.75)

write_csv(tips2, "RestaurantTips2.csv")
