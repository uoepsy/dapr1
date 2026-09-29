# 07 -----

# Step 1: load and inspect data
# Read the data into R
library(tidyverse)
freshers <- read_csv("https://uoepsy.github.io/data/dapr1-freshers-data.csv")
# Look at the structure and summary
glimpse(freshers)
summary(freshers$Stats_Anxiety)
summary(freshers$Cog_Aptitude)

# Step 2: calculating z-scores
# Method 1: using mutate() from tidyverse
freshers <- freshers |>
  mutate(
    Z_Anxiety = (Stats_Anxiety - mean(Stats_Anxiety)) / sd(Stats_Anxiety)
  )
# Method 2: manual calculation using the formula
freshers$Z_Aptitude <- (freshers$Cog_Aptitude - mean(freshers$Cog_Aptitude)) /
  sd(freshers$Cog_Aptitude)
# Check the mean and SD of our new Z-scores
mean(freshers$Z_Anxiety)
sd(freshers$Z_Anxiety)
mean(freshers$Z_Aptitude)
sd(freshers$Z_Aptitude)

# Step 3: using pnorm for probabilities
# What's the probability of a student having a Stats Anxiety score above 80?
# First, find the Z-score for 80
Z_80 <- (80 - mean(freshers$Stats_Anxiety)) / sd(freshers$Stats_Anxiety)
# Now, find the probability (area to the right)
prob_high_anxiety <- 1 - pnorm(Z_80)
prob_high_anxiety

# Step 4: using qnorm for quantiles
# The department wants to give a "High Aptitude" award to the top 10% of students.
# What is the raw score cutoff for the top 10%?
# 1. Find the Z-score for the 90th percentile (top 10% means 0.90 are below it)
Z_cutoff <- qnorm(0.90)
Z_cutoff
# 2. Back-transformation to raw score X = M + (Z * SD)
X_cutoff <- mean(freshers$Cog_Aptitude) + (Z_cutoff * sd(freshers$Cog_Aptitude))
X_cutoff

# 08 -----

# 1. Read data into R and inspect it
library(tidyverse)
population_data <- read_csv("https://uoepsy.github.io/data/dapr1-population-wellbeing.csv")
glimpse(population_data)
# 2. Visualise population distribution and calculate population parameters (we couldn't in practice)
ggplot(population_data, aes(x = Wellbeing)) +
  geom_histogram(bins = 40, fill = "green4", colour = "black") +
  labs(x = "Wellbeing Score (0-100)",
       y = "Frequency") +
  theme_light(base_size = 16)
mean(population_wellbeing) # population mean
sd(population_wellbeing) # population SD

# 3. Simulate sampling distribution for n=10 (small sample size)
set.seed(5678)
n_small <- 10
num_simulations <- 1000
df10 <- tibble(
  SampleMean = replicate(num_simulations,
                         mean(sample(population_wellbeing, n_small)))
)
ggplot(df10, aes(x = SampleMean)) +
  geom_histogram(bins = 30, fill = "cornflowerblue", colour = "black") +
  labs(x = "Sample Mean Wellbeing Score",
       y = "Frequency",
       title = "Sampling Distribution of the Mean (each sample n=10)") +
  theme_light(base_size = 16)
SE_n10 <- sd(df10$SampleMean)
SE_n10

# 4. Simulate sampling distribution for n=50 (large sample size)
set.seed(6789)
n_large <- 50
num_simulations <- 1000
df50 <- tibble(
  SampleMean = replicate(num_simulations,
                         mean(sample(population_wellbeing, n_large)))
)
ggplot(df50, aes(x = SampleMean)) +
  geom_histogram(bins = 30, fill = "cornflowerblue", colour = "black") +
  labs(x = "Sample Mean Wellbeing Score",
       y = "Frequency",
       title = "Sampling Distribution of the Mean (each sample n=50)") +
  theme_light(base_size = 16)
SE_n50 <- sd(df50$SampleMean)
SE_n50

# 5. Check the math: SE = Population SD / sqrt(n)
tibble(
  SE_n10 = SE_n10,
  Theoretical_SE_n10 = sd(population_wellbeing) / sqrt(n_small)
)
tibble(
  SE_n50 = SE_n50,
  Theoretical_SE_n50 = sd(population_wellbeing) / sqrt(n_large)
)

# 09 -----
# TBD

# 10 -----

# 1. Read the data into R and inspect it
library(tidyverse)
bilingualism_data <- read_csv("https:/uoepsy.github.io/data/dapr1-bilingualism-data.csv")
glimpse(bilingualism_data)
# 2. Compute the test statistic
x_bar <- mean(bilingualism_data$iq_score) # Point Estimate (Sample Mean)
x_bar
sigma <- 15 n <- nrow(bilingualism_data) # Known population SD
# Sample size (n)
n
SE
SE <- sigma / sqrt(n)
Z <- (x_bar - 100) / SE
Z

# 3. Compute the p-value
pvalue <- 1 - pnorm(Z)
pvalue
# 4. Make a decision by comparing the p-value to alpha
alpha <- 0.05
pvalue <= alpha
# If H1 : mu < 100
pnorm(Z)
# If H1 : mu not = 100, p-value = twice the area to the right of the absolute value of Z
2 * (1 - pnorm( abs(Z)) )



# 11 -----
# basically just t.test()