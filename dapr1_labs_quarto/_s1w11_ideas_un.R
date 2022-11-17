# =====
# Ideas for s1w11 lab - UN
# Sampling distributions
# =====


# Notes for lab are below
# =====

rm(list = ls())

library(tidyverse)
library(patchwork)

tips <- read_csv("https://uoepsy.github.io/data/RestaurantTips.csv")
head(tips)


# Data prep
# =====

# Outlier
ggplot(tips, aes(x = PctTip)) + 
    geom_histogram(color = 'white')

# Inspect outlier - who tipped more than the total bill?
tips %>%
    filter(PctTip > 100)

# Missing Tip (NA) - perhaps PctTip had an input error? The 2 could be double-typed?

# Remove outlier
tips = tips %>%
    filter(PctTip <= 100)

# Use better labels
tips$Day <- factor(tips$Day, 
                   levels = c("m", "t", "w", "th", "f"),
                   labels = c("Mon", "Tue", "Wed", "Thu", "Fri"))

# Sample distribution of PctTip
ggplot(tips, aes(x = PctTip)) + 
    geom_histogram(color = 'white')

ggplot(tips, aes(x = PctTip)) + 
    geom_density()


# Sample distribution of the mean
# - We will focus on the distribution of the average PctTip
# =====

# Mean, SE
n <- nrow(tips)
n
xbar <- mean(tips$PctTip)
xbar
se <- sd(tips$PctTip) / sqrt(n)
se

# xbar ~ N(16.59, 0.35)


# Note the variability of means is < variability of sample data
sd(tips$PctTip)
se


# How does this vary across days?

tbl <- tips %>%
    group_by(Day) %>% 
    summarise(n = n(),
              xbar = mean(PctTip),
              se = sd(PctTip) / sqrt(n)) %>%
    mutate(xbar = round(xbar, 2),
           se = round(se, 2)) %>% 
    as.data.frame()
tbl

# xbar_Mon ~ N(15.94, 0.72)
# xbar_Tue ~ N(18.02, 2.11)
# xbar_Wed ~ N(16.55, 0.44)
# xbar_Thu ~ N(16.75, 0.57)
# xbar_Fri ~ N(16.26, 1.21)

# Interpret Mu
#   Tue has the highest average pcttip, mon the lowest. 
#   Tue is the highest SE, followed by Fri.
#   Apart from Tue, the other working days are close to the average tipping rate (15%),
#   while on Tue the average tipping rate is more likely to be above average

# Interpret SE
#   We expect pcttip means to vary the most on Tuesdays and Fridays, i.e. in these days
#   we are more likely to obtain tips that are either very generous or very stingy


# Create plot with ggplot or table, are they similar?

plt1 <- ggplot(tips, aes(x = Day, y = PctTip)) + 
    stat_summary(fun.data = function(x) mean_se(x, 2))
plt1

plt2 <- ggplot(tbl) +
    geom_pointrange(aes(x = Day, y = xbar,
                        ymin = xbar - 2 * se,
                        ymax = xbar + 2 * se))
plt2

plt1 | plt2 # The plots match!


# Other way?
ggplot(tips, aes(x = PctTip, y = Day)) + 
    stat_summary(fun.data = function(x) mean_se(x, 2))


# Compare variability of means to that of data?
plt3 <- ggplot(tips, aes(x = PctTip, y = Day)) + 
    stat_summary(fun.data = function(x) mean_se(x, 2)) +
    xlim(0, 100)
plt3

# How good is the Mean, SE when the distribution is skewed?
library(ggridges)

plt4 <- ggplot(tips, aes(x = PctTip, y = Day)) + 
    geom_density_ridges() +
    xlim(0, 100)
plt4

plt3 / plt4


# SE < SD

tips %>%
    group_by(Day) %>% 
    summarise(n = n(),
              xbar = mean(PctTip),
              SD = sd(PctTip),
              SE = SD / sqrt(n)) %>%
    mutate(IsSESmaller = SE < SD)
