setwd("~/Documents/uoepsy/dapr1/internal_data/sem1")
library(tidyverse)
set.seed(123456)

df = read_csv("HollywoodMovies.csv")

glimpse(df)

df = df %>% 
    filter(!str_detect(LeadStudio, "Weinstein"))

glimpse(df)


# 1-5
# read data, NA

df$LeadStudio %>% table() %>% sort()
df$Genre %>% table() %>% sort()

# top 5 studios
df.2 = df %>%
    filter(LeadStudio %in% c("Warner Bros.", "Universal Pictures", "Lionsgate",
                             "Twentieth Century Fox", "Paramount Pictures"))

# top 10 genres
df.2 = df.2 %>%
    filter(!(Genre %in% c("Concert", "Western")))

with(df.2, table(Genre, LeadStudio)) %>% prop.table(margin = 2) %>% 
    addmargins() %>% round(2)

glimpse(df.2)

df.2 %>%
    ggplot(aes(Genre, RottenTomatoes)) +
    geom_boxplot()

# corr week
df.2 %>%
    ggplot(aes(AudienceScore, RottenTomatoes)) +
    geom_point() +
    geom_smooth(method = 'lm')

# normal data
N = nrow(df.2)
for (i in 1:50) {
    df.2[, sprintf('IQ%0d', i)] = rnorm(N, 100, 15)
}

df.2 %>%
    pivot_longer(IQ1:IQ50, names_to = 'Viewer', values_to = 'ViewersIQ')


# binomial data
df.2$Snacks = rbinom(N, 50, 0.49)
df.2$PrivateTransport = rbinom(N, 50, 0.7)

barplot(table(df.2$Snacks))
barplot(table(df.2$PrivateTransport))



# E(X) = n * p
# SD(X) = sqrt(n * p * (1-p))
p = mean(df.2$Snacks) / 50
sd.x = sqrt(50 * p * (1-p))
grid.x = seq(min(df.2$Snacks), max(df.2$Snacks), 1)
grid.y = dbinom(grid.x, 50, p)
# library(fitdistrplus)
# fitdist(df.2$Snacks, "binom", start = list(prob = 0.5), fix.arg = list(size = N))

# barplot(table(df.2$Snacks), density = TRUE)
# points(grid.x, grid.y, col = 'red')

ggplot() +
    geom_bar(data = df.2, aes(x = Snacks, y = ..prop..)) +
    geom_point(data = tibble(grid.x, grid.y), aes(grid.x, grid.y), color = 'red')


write_csv(df.2, file = "hollywood_movies_subset.csv")
