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

testdf = outdf
while(TRUE){
  if(shapiro.test(testdf$outlook)$p.value > .05){
    if(nrow(testdf)>40){
      break
    }
  }
  toadd = slice_sample(outdf,n=1) |>
    mutate(
      pseudonym=NA,
      birthmonth = sample(tolower(month.name),1),
      n_sibling = rpois(1,1),
      eye_colour = 
        sample(c("brown","blue","green","hazel","amber"),1,
               prob = c(.5,.2,.1,.09,.01)),
      distance_born = rgamma(1,shape=3,scale=5e2),
      outlook = round(rnorm(1,
                            mean(testdf$outlook),
                            sd(testdf$outlook))),
      ampm = round(rnorm(1,mean(testdf$ampm,ampm=T),
                         sd(testdf$ampm,na.rm=T)),1),
      sleepqual = round(rnorm(1,mean(testdf$sleepqual,na.rm=T),
                              sd(testdf$sleepqual,na.rm=T))),
      procrast = round(rnorm(1,mean(testdf$procrast,na.rm=T),
                             sd(testdf$procrast,na.rm=T))),
      multitask = round(rnorm(1,mean(testdf$multitask,na.rm=T),
                              sd(testdf$multitask,na.rm=T))),
      threewords = NA
      )
      testdf = bind_rows(testdf,toadd) |> slice_sample(prop=1)
}

dim(testdf)
hist(testdf$outlook,breaks=20)
# summary(testdf |> mutate_if(is.character,as.factor))
# 
# testdf |> select_if(is.numeric) |> psych::pairs.panels()

# readr::write_csv(testdf,file="../../data/dapr1_2526_survey.csv")
