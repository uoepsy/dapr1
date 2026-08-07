library(qualtRics)
library(tidyverse)

df <- fetch_survey("SV_7VxqkFo12wBita6") |> 
  janitor::clean_names() |>
  filter(status != "Survey Preview")

outdf <- tibble(
  pseudonym = tolower(df$q2),
  birthmonth = tolower(df$q3),
  n_sibling = as.numeric(df$q4),
  spicy = tolower(sub(":.*","",df$q7)),
  spicy_num = dplyr::case_when(
    spicy == "anti-spice" ~ 1,
    spicy == "mild" ~ 2,
    spicy == "medium" ~ 3,
    spicy == "hot" ~ 4,
    spicy == "very hot" ~ 5,
    spicy == "extreme" ~ 6,
    TRUE ~ NA
  ),
  eye_colour = tolower(df$q8),
  distance_born = as.numeric(df$q9),
  outlook = as.numeric(df$q10_1),
  ampm = as.numeric(df$q11_1),
  sleepqual = as.numeric(df$q12_1),
  procrast = as.numeric(df$q13_1),
  multitask = as.numeric(df$q14_1),
  fav_color = tolower(df$q15),
  catdog = tolower(df$q16),
  threewords = tolower(df$q17)
)

readr::write_csv(df,file="../../data/dapr1_2526_survey.csv")
