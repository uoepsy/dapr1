library(MASS)
library(tidyverse)

# Lectures 2-4
set.seed(7284)
mu <- c(66, 72)
Sigma <- matrix(c(7, 3.75,
                  3.75, 6), byrow = T, ncol = 2)

rawvars <- mvrnorm(n=10000, mu=mu, Sigma=Sigma)

set.seed(7284)
df <- rawvars[sample(nrow(rawvars), 150),]

set.seed(7284)
ex1 <- tibble(
  ID = c(paste("ID", 101:250, sep = "")),
  Degree = factor(sample(c("Psych", "Ling", "Phil", "Joint"), 
                         150, replace=T, prob=c(.2,.5,.2,.1))),
  Year = sample(1:4, 150, replace = T, prob = c(0.2, 0.5, 0.2, 0.1)),
  Score1 = round(df[,1], 0),
  Score2 = round(df[,2], 0)
)
write_csv(ex1, "C:/Work/Teaching/psychstats/dapr1/dapr1_lectures/ex1.csv")

