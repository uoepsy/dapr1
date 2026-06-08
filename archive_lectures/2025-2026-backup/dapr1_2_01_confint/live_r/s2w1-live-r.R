library(tidyverse)

temp_data <- read_csv('https://uoepsy.github.io/data/BodyTemp.csv')

dim(temp_data)
head(temp_data)

ggplot(temp_data, aes(x=BodyTemp))+
    geom_histogram()+
    labs(x="Body temperature (°C)")

ggplot(temp_data, aes(x=BodyTemp))+
    geom_density()+
    labs(x="Body temperature (°C)")

ggplot(temp_data, aes(x=BodyTemp))+
    geom_boxplot()+
    labs(x="Body temperature (°C)")

# xbar - t* se
# xbar + t* se

xbar <- mean(temp_data$BodyTemp)
xbar

s <- sd(temp_data$BodyTemp)
s

n <- nrow(temp_data)
n

se <- s / sqrt(n)
se

tstar <- qt(c(0.025, 0.975), df = n - 1)
tstar

xbar + tstar * se
