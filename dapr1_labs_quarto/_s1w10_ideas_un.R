## Ideas for s1w10 lab - UN

library(tidyverse)
tips2 <- read_csv("https://uoepsy.github.io/data/RestaurantTips2.csv")
head(tips2)

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

ggplot(tips2_long, aes(x = IQ)) + 
    geom_histogram(color = 'white')

mu_hat = mean(tips2_long$IQ)
sigma_hat = sd(tips2_long$IQ)

# dnorm(x, mu, sigma) = normal distribution N(mu, sigma) at x
normal_distr <- tibble(
    x = seq( mu_hat - 4 * sigma_hat, mu_hat + 4 * sigma_hat, by = 0.1 ),
    y = dnorm( x, mean = mu_hat, sd = sigma_hat )
)

# plot sample distribution vs estimated/fitted distribution
ggplot() + 
    geom_histogram(data = tips2_long, aes(x = IQ, y = after_stat(density)),
                   color = 'white') +
    geom_line(data = normal_distr, aes(x = x, y = y), 
              color = 'red', linewidth = 1)

# Probabilities
# P(X <= x)
pnorm(100, mu_hat, sigma_hat)

# P(X >= x) = P(X > x)
1 - pnorm(100, mu_hat, sigma_hat)
pnorm(100, mu_hat, sigma_hat, lower.tail = FALSE)

# P(a < X < b) = P(X < b) - P(X < a)
pnorm(100, mu_hat, sigma_hat) - pnorm(80, mu_hat, sigma_hat)


# dnorm is not a probability in continuous distribution, it 
# just gives you the density = the height of the normal curve at a specific x

# Probabilites for continuous random variables are calculates as the probability
# that the random variable takes a value within a specific interval, as 
# the probability that it takes on any specific value is 0, P(X = a) = 0

# Functions:
# dnorm, pnorm, mutate, pivot_longer, paste, relocate, rename (if you want?),
# seq, geom_histogram, geom_line or stat/geom_function

# Readings:
# Week 10, 11 readings to be split from former labs.
