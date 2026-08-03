# 02 ----

library(tidyverse)
mdd_data <- read_csv("https://uoepsy.github.io/data/dapr1-mdd-data.csv")
head(mdd_data)
dim(mdd_data) # dimensions: number of rows and columns
nrow(mdd_data) # number of rows
ncol(mdd_data) # number of columns
str(mdd_data) 
glimpse(mdd_data)
# glimpse and str: structure of data (columns, type, and preview)
# Recording categorical variables (called factors in R code)
mdd_data |>
  glimpse(mdd_data)


mutate(Treatment = factor(Treatment))
mdd_data <- mdd_data |>
  mutate(Treatment = factor(Treatment))
glimpse(mdd_data)
mdd_data <- mdd_data |>
  mutate(
    Treatment_Label = factor(Treatment_num,
                             levels = c(0, 1, 2),
                             labels = c("placebo", "ADM", "CT")
    )
    
# ^ DONE
    
# 03 ----

library(tidyverse)
mdd_data <- read_csv("https://uoepsy.github.io/data/dapr1-mdd-data.csv")
mdd_data <- mdd_data |>
  mutate(Treatment = factor(Treatment))
glimpse(mdd_data)
mdd_data |>
  count(Treatment) |>
  mutate(f = n / sum(n), Percent = f * 100)
ggplot(mdd_data, aes(x = Treatment)) + geom_bar(fill = 'darkblue') +
  labs(x = "Treatment")
# y = Treatment for horizontal barplot
ggplot(mdd_data, aes(x = HDRS8)) +
  geom_histogram(fill = 'darkgreen', colour = 'black') +
  labs(x = "HDRS8")
ggplot(mdd_data, aes(x = HDRS8)) +
  geom_density(colour = 'red') +
  labs(x = "HDRS8")

# 04 ----

library(tidyverse)
mdd_data <- read_csv("data/dapr1-mdd-data.csv")
mdd_data <- mdd_data |>
  mutate(Treatment = factor(Treatment))
glimpse(mdd_data)
mdd_data |>
  count(Treatment)
mdd_data |>
  group_by(Treatment) |>
  summarise(n = n())
mdd_data |>
  count(Treatment) |>
  mutate(f = n / sum(n), Percent = f * 100)
mdd_data |>
  summarise(
    Med = median(HDRS8),
    Mean = mean(HDRS8) # Mean = sum(HDRS8) / n()
  )
ggplot(mdd_data, aes(x = HDRS8)) +
  geom_boxplot() +
  labs(x = "HDRS8 scores")

# 05 ----

library(tidyverse)
mdd_data <- read_csv("data/dapr1-mdd-data.csv")
mdd_data <- mdd_data |>
  mutate(Treatment = factor(Treatment))
glimpse(mdd_data)
mdd_data |> summarise(
  R = max(HDRS8) - min(HDRS8),
  Q1 = quantile(HDRS8, 0.25),
  Q3 = quantile(HDRS8, 0.75),
  IQR = IQR(HDRS8), # or IQR = Q3 –
  Q1
  Var = var(HDRS8),
  SD = sd(HDRS8),
)
mdd_data |>
  summarise(
    n = n(),
    M = mean(HDRS8),
    SD = sd(HDRS8)
  ) # for symmetric distributions
mdd_data |>
  summarise(
    n = n(),
    Med = median(HDRS8),
    IQR = IQR(HDRS8)
  ) # for asymmetric distributions
mdd_data |>
  group_by(Treatment) |>
  summarise(
    n = n(),
    M = mean(HDRS8),
    SD = sd(HDRS8)
  ) # descriptives by group