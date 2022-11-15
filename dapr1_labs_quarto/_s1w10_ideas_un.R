# =====
# Ideas for s1w10 lab - UN
# Continuous random variables and the Normal distribution
# =====


# Notes for lecture
# =====

# - Ensure students know that P(X = x) = 0
# - There is no difference in the result of P(a <= X <= b) and P(a < X < b)
# - Ensure notation is consistent for Binomial pmf and Normal pdf. Currently
#   Binomial uses f(k, n, p) while Normal uses f(x | mu, sigma). I would opt for
#   f(k; n, p) and f(x; mu, sigma) as per Mood, Graybill, and Boes where they 
#   put the parameters of the distribution after a semicolon


# Notes for lab are below
# =====

rm(list = ls())

library(tidyverse)
tips2 <- read_csv("https://uoepsy.github.io/data/RestaurantTips2.csv")
head(tips2)


# Context
# =====

# - Add some explanation of columns IQ1 and IQ1, e.g. that each party guest was 
#   additionally asked to take an IQ test.
# - Pivot longer is used to obtain a single column of IQ scores
# - Discuss how this column can be considered an independent sample of IQ scores


# Data prep
# =====

# Option 1
tips2 <- tips2 %>%
    mutate(PartyID = 1:n())

tips2_long <- tips2 %>%
    pivot_longer(IQ1:IQ2, names_to = "ID", values_to = "IQ") %>%
    mutate(ID = str_replace(ID, "IQ", ""))
tips2_long

tips2_long <- tips2_long %>%
    mutate(UniqueID = paste(PartyID, ID, sep=".")) %>%
    select(-PartyID, -ID) %>%
    relocate(UniqueID, .before = Bill)
tips2_long

# Option 2
tips2_long <- tips2 %>%
    pivot_longer(IQ1:IQ2, names_to = "ID", values_to = "IQ")
tips2_long


# Sample distribution
# =====

ggplot(tips2_long, aes(x = IQ)) + 
    geom_histogram(color = 'white')


# Fit Normal distribution
# =====

# Fitting a Normal distribution to the sample data requires estimating the 
# parameters mu and sigma using the sample mean and sample standard deviation
mu_hat = mean(tips2_long$IQ)
sigma_hat = sd(tips2_long$IQ)

# dnorm
# * I would tell students that dnorm means the normal distribution/curve at x
#   i.e. this is not a probability but the density (do they see density in lecture?)
# * dnorm(x, mean = mu_hat, sd = sigma_hat) = normal distribution 
#   N(mu_hat, sigma_hat) at x

# Plot fitted Normal distribution on top of the sample distribution
# * Grid of x values for plotting - Use mean - 4*SD, mean + 4*SD to cover 
#   100% of the values. 
# * Usually mean - 3*SD, mean + 3*SD covers 100% of values but in this case 
#   one escaped!
# * y_grid is the Normal curve at x_grid
normal_distr <- tibble(
    x_grid = seq( mu_hat - 4 * sigma_hat, mu_hat + 4 * sigma_hat, by = 0.1 ),
    y_grid = dnorm( x_grid, mean = mu_hat, sd = sigma_hat )
)

# Plot sample distribution vs fitted Normal distribution
ggplot() + 
    geom_histogram(data = tips2_long, aes(x = IQ, y = after_stat(density)),
                   color = 'white') +
    geom_line(data = normal_distr, aes(x = x_grid, y = y_grid), 
              color = 'red', linewidth = 1)

# Is the Normal distribution a good fit? 
# =====

# I.e. do the sample distribution and the Normal fit tend to agree?


# Computing probabilities
# =====

# Please change any of the values x to any value you deem best for the context, 
#   here 100 is just an example

# P(X <= x) = P(X < x) for continuous RVs
pnorm(100, mu_hat, sigma_hat)

# P(X >= x) = P(X > x)
1 - pnorm(100, mu_hat, sigma_hat)
pnorm(100, mu_hat, sigma_hat, lower.tail = FALSE)

# P(a < X < b) = P(X < b) - P(X < a)
pnorm(100, mu_hat, sigma_hat) - pnorm(80, mu_hat, sigma_hat)


# Quantiles
# =====

# Example, what is the IQ value such that 25% of the guests have an IQ 
#   less than or equal to that specific value?
# x such that P(X <= x) = 0.25, this is the first quartile
qnorm(0.25, mean = mu_hat, sd = sigma_hat)

# Example, what is the IQ value such that 50% of the guests have an IQ 
#   less than or equal to that specific value?
# x such that P(X <= x) = 0.5, this is the median
qnorm(0.5, mean = mu_hat, sd = sigma_hat)

# Example, what is the IQ value such that 75% of the guests have an IQ 
#   less than or equal to that specific value?
# x such that P(X <= x) = 0.75, this is the third quartile
qnorm(0.75, mean = mu_hat, sd = sigma_hat)

# Quartiles are the 25th, 50th, 75th percentiles.
# How do the Normal quartiles computed above compare to the summary ones? 
#   Do they agree?
summary(tips2_long$IQ)

# What is the interval comprising 95% of the people's IQ scores in the sample?
#   0.975 - 0.025 = 0.95
qnorm(c(0.025, 0.975), mean = mu_hat, sd = sigma_hat)


# Important points
# =====
# Discuss why do we not use dnorm for computing probabilities
#   dnorm is not a probability in continuous distribution, it 
#   just gives you the density = the height of the normal curve at a specific x
# 
#   Probabilities for continuous random variables are calculated as the probability
#   that the random variable takes a value within a specific interval, as 
#   the probability that it takes on any specific value is 0, P(X = a) = 0


# Functions
# =====
#   dnorm, pnorm, mutate, pivot_longer, paste, relocate, rename (if you want?),
#   seq, geom_histogram, geom_line or stat/geom_function


# Readings
# =====
#   Week 10 reading needs to be split from the former lab.
