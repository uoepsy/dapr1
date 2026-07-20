df <-readxl::read_xlsx("DAPR1 Welcome Survey!(1-3).xlsx")[,7:19]

names(df) <- c(
  "pseudonym",
  "birthmonth",
  "am_pm",
  "n_sibling",
  "spicy",
  "eye_colour",
  "distance_born",
  "sleepqual",
  "procrast",
  "multitask",
  "fav_colour",
  "catdog",
  "threewords"
)

df <- df |> dplyr::transmute(
  pseudonym = tolower(pseudonym),
  birthmonth = tolower(birthmonth),
  am_pm = tolower(am_pm),
  n_sibling = as.numeric(n_sibling),
  spicy = tolower(sub(":.*","",spicy)),
  spicy_num = dplyr::case_when(
    spicy == "anti-spice" ~ 1,
    spicy == "mild" ~ 2,
    spicy == "medium" ~ 3,
    spicy == "hot" ~ 4,
    spicy == "very hot" ~ 5,
    spicy == "extreme" ~ 6,
    TRUE ~ NA
  ),
  eye_colour = tolower(eye_colour),
  distance_born = as.numeric(distance_born),
  sleepqual = as.numeric(sleepqual),
  procrast = as.numeric(procrast),
  multitask = as.numeric(multitask),
  fav_color = tolower(fav_colour),
  catdog = tolower(catdog),
  threewords = tolower(threewords)
)

readr::write_csv(df,file="../../data/dapr1_2526_survey.csv")
