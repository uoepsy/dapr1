library(tidyverse)
df = read_csv("https://uoepsy.github.io/data/hollywood_movies_subset.csv")
summary(df)

with(df, table(Genre, LeadStudio)) %>%
    prop.table(margin = 1) %>%
    addmargins() %>%
    round(2)

head(df)
str(df)
plot(RottenTomatoes ~ AudienceScore, data = df)
