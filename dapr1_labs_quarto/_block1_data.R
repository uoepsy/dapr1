rm(list = ls())

# week 1
library(tidyverse)
movies <- read_csv("https://uoepsy.github.io/data/hollywood_movies_subset.csv")
head(movies)
nrow(movies)
ncol(movies)
dim(movies)
glimpse(movies)
summary(movies)

# week 2
library(patchwork)
library(kableExtra)

movies <- movies %>%
    select(1:15)

freqGenre <- ggplot(movies, aes(x = Genre)) +
    geom_bar() +
    labs(x = "Movie genre", y = "Frequency") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
freqLeadStudio <- ggplot(movies, aes(x = LeadStudio)) +
    geom_bar() +
    labs(x = "Lead studio", y = "Frequency") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
freqGenre | freqLeadStudio

ggplot(movies) +
    geom_bar(aes(x = fct_infreq(Genre))) +
    labs(x = "Movie genre", y = "Frequency") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(movies, aes(x = fct_infreq(Genre))) +
    geom_bar() +
    labs(x = "Movie genre", y = "Frequency") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(movies) +
    geom_bar(aes(x = fct_rev(fct_infreq(Genre)))) +
    labs(x = "Movie genre", y = "Frequency") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(movies) +
    geom_bar(aes(x = fct_infreq(Genre))) +
    labs(x = "Movie genre", y = "Frequency") +
    coord_flip()

ggplot(movies) +
    geom_bar(aes(x = fct_rev(fct_infreq(Genre)))) +
    labs(x = "Movie genre", y = "Frequency") +
    coord_flip()

movies %>% 
    count(Genre, name = 'Freq') %>% 
    mutate(Perc = Freq / sum(Freq) * 100) %>% 
    mutate(Perc = round(Perc, 2)) %>% 
    arrange(desc(Freq)) %>%
    kbl(booktabs = TRUE)

movies %>% 
    count(LeadStudio, name = 'Freq') %>% 
    mutate(Perc = Freq / sum(Freq) * 100) %>% 
    mutate(Perc = round(Perc, 2)) %>% 
    arrange(desc(Freq)) %>%
    kbl(booktabs = TRUE)


# week 3

glimpse(movies)

library(GGally)
library(patchwork)

movies %>%
    select(where(is.numeric)) %>%
    ggpairs()

p <- movies %>%
    select(where(is.numeric)) %>%
    map( ~ggplot(movies, aes(x = .)) + geom_histogram() )
wrap_plots(p, 4, 3)

# p1 <- ggplot(movies, aes(x = RottenTomatoes)) + 
#     geom_histogram() + 
#     labs(x = "Rotten Tomatoes Rating")
# p2 <- ggplot(movies, aes(x = AudienceScore)) + 
#     geom_histogram() + 
#     labs(x = "Audience Score")
# p3 <- ggplot(movies, aes(x = TheatersOpenWeek)) + 
#     geom_histogram() + 
#     labs(x = "Theaters Open Week")
# p4 <- ggplot(movies, aes(x = OpeningWeekend)) + 
#     geom_histogram() + 
#     labs(x = "Opening Weekend")
# 
# p5 <- ggplot(movies, aes(x = BOAvgOpenWeekend)) + 
#     geom_histogram() + 
#     labs(x = "BOAvgOpenWeekend")
# p6 <- ggplot(movies, aes(x = Budget)) + 
#     geom_histogram() + 
#     labs(x = "Budget")
# p7 <- ggplot(movies, aes(x = DomesticGross)) + 
#     geom_histogram() + 
#     labs(x = "DomesticGross")
# p8 <- ggplot(movies, aes(x = WorldGross)) + 
#     geom_histogram() + 
#     labs(x = "WorldGross")
# 
# p9 <- ggplot(movies, aes(x = ForeignGross)) + 
#     geom_histogram() + 
#     labs(x = "ForeignGross")
# p10 <- ggplot(movies, aes(x = Profitability)) + 
#     geom_histogram() + 
#     labs(x = "Profitability")
# p11 <- ggplot(movies, aes(x = OpenProfit)) + 
#     geom_histogram() + 
#     labs(x = "OpenProfit")
# p12 <- ggplot(movies, aes(x = Year)) + 
#     geom_histogram() + 
#     labs(x = "Year")
# 
# wrap_plots(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, 
#            nrow = 4, ncol = 3)

long_movies <- movies %>%
    select(where(is.numeric)) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

ggplot(long_movies, aes(x = value)) +
    geom_histogram(color = 'white') +
    facet_wrap(~ variable, scales = "free") +
    labs(x = "", y = "Count")

ggplot(long_movies, aes(y = value)) +
    geom_boxplot() +
    facet_wrap(~ variable, scales = "free") +
    labs(x = "", y = "Count")

long_movies %>%
    group_by(variable) %>%
    summarise(M = mean(value, na.rm = TRUE),
              SD = sd(value, na.rm = TRUE))




# -------------------------------------------

# library(tidyverse)
# df = read_csv("https://uoepsy.github.io/data/hollywood_movies_subset.csv")
# summary(df)
# 
# with(df, table(Genre, LeadStudio)) %>%
#     prop.table(margin = 1) %>%
#     addmargins() %>%
#     round(2)
# 
# head(df)
# str(df)
# plot(RottenTomatoes ~ AudienceScore, data = df)

