library(qualtRics)
library(tidyverse)

make_norm <- function(x){
  if(shapiro.test(x)$p.value >= .05){
    return(x)
  }
  sx = sd(x)
  optx = x
  optp = shapiro.test(x)$p.value
  
  nscale = 0.01
  while(TRUE){
    addnoise <- rnorm(length(x), 0, sd = sx*nscale)
    testx = x + addnoise
    testp = shapiro.test(testx)$p.value
    if(testp > optp) {
      optp = testp
      optx = testx
    }
    if(optp >= .05){
      cat(paste0("noise",nscale))
      return(optx)
    }
    # increase noise if not
    nscale = nscale + .001
  }
}

df <- fetch_survey("SV_7VxqkFo12wBita6")[-c(1:10),] |> 
  janitor::clean_names() |>
  filter(status != "Survey Preview") |>
  filter(progress > 90)

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
  if(shapiro.test(testdf$outlook)$p.value >= .05){
    break
  }
  cat(paste0("not yet\n"))
  # bound at -100,100
  testdf$outlook <- 
    pmin(100,pmax(-100,round(make_norm(testdf$outlook))))
  # preserve some whole things: 
  tokeep = c(0,50,80,35,-10,10,30,100,-100,-50)
  testdf$outlook[outdf$outlook %in% tokeep] <-
    outdf$outlook[outdf$outlook %in% tokeep]  
}

hist(testdf$outlook)
qqnorm(testdf$outlook);qqline(testdf$outlook)
shapiro.test(testdf$outlook)

head(testdf)

# summary(testdf |> mutate_if(is.character,as.factor) |> select_if(is.factor))
# testdf |> select_if(is.numeric) |> psych::pairs.panels()
# readr::write_csv(testdf,file="../../data/dapr1_2627_survey.csv")


# 
# 
# 
# 
# testdf = outdf
# while(TRUE){
#   if(shapiro.test(testdf$outlook)$p.value > .05){
#     if(nrow(testdf)>40){
#       if(mean(testdf$outlook)>30) { break }
#     }
#   }
#   toadd = slice_sample(outdf,n=1) |>
#     mutate(
#       pseudonym=NA,
#       birthmonth = sample(tolower(month.name),1),
#       n_sibling = rpois(1,1),
#       eye_colour = 
#         sample(c("brown","blue","green","hazel","amber"),1,
#                prob = c(.5,.2,.1,.09,.01)),
#       distance_born = rgamma(1,shape=3,scale=5e2),
#       outlook = round(rnorm(1,
#                             mean(testdf$outlook),
#                             sd(testdf$outlook)/2)),
#       ampm = round(rnorm(1,mean(testdf$ampm,ampm=T),
#                          sd(testdf$ampm,na.rm=T)),1),
#       sleepqual = round(rnorm(1,mean(testdf$sleepqual,na.rm=T),
#                               sd(testdf$sleepqual,na.rm=T))),
#       procrast = round(rnorm(1,mean(testdf$procrast,na.rm=T),
#                              sd(testdf$procrast,na.rm=T))),
#       multitask = round(rnorm(1,mean(testdf$multitask,na.rm=T),
#                               sd(testdf$multitask,na.rm=T))),
#       threewords = NA
#       ) |>
#     mutate(
#       outlook = pmin(100,pmax(-100,outlook)),
#       ampm = pmin(10,pmax(0,ampm)),
#       sleepqual = pmin(100, pmax(0, sleepqual)),
#       procrast = pmin(100, pmax(0, procrast)),
#       multitask = pmin(100, pmax(0, multitask))
#     )
#   testdf = bind_rows(testdf,toadd) |> slice_sample(prop=1)
# }


# summary(testdf |> mutate_if(is.character,as.factor))
# 
# testdf |> select_if(is.numeric) |> psych::pairs.panels()

# readr::write_csv(testdf,file="../../data/dapr1_2526_survey.csv")

