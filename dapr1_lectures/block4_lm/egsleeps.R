### EG1 continuous rct ------
set.seed(65683728)
eg1sleep <- tibble(
  person = unlist(lapply(1:5, \(x) paste0(LETTERS[x],LETTERS)))[1:80],
  n_disrupted = rep(0:7,e=10),
  psolve = (58 - 2 * n_disrupted) + 
    residuals(lm(rnorm(80,0,15) ~ rep(0:7,e=10)))
) |> slice_sample(prop=1) |> select(-person)


### EG2 binary rct ------
set.seed(65683728)
xbin = sample(c("Normal","Interrupted"),80,T,prob=c(.6,.4))
eg2sleep <- tibble(
  condition = xbin,
  psolve = (50 + 7.5 * (condition=="Normal")) + 
    residuals(lm(rnorm(80,0,7) ~ xbin))
)
set.seed(356)
eg2sleep <- slice_sample(eg2sleep, prop=1)



### EG3 obs 
set.seed(65683728)
n <- 80
drink = sample(c("No","Yes"),n,TRUE,prob=c(.4,.6))
xbin = ifelse(drink=="Yes",
              sample(c("Good", "Bad"), n, replace = TRUE, prob = c(0.2, 0.8)),
              sample(c("Good", "Bad"), n, replace = TRUE, prob = c(0.8, 0.2)))
sleep_good <- as.numeric(xbin == "Good")
drink_yes  <- as.numeric(drink == "Yes")

raw_noise <- rnorm(n, 0, 7)
noise <- numeric(n)
noise[drink == "No"]  <- residuals(lm(raw_noise ~ sleep_good, subset = (drink == "No")))
noise[drink == "Yes"] <- residuals(lm(raw_noise ~ sleep_good, subset = (drink == "Yes")))
bd <- -8 
b_scond <- 5 - bd * coef(lm(drink_yes ~ sleep_good))[2]
eg3sleep <- tibble(
  person = unlist(lapply(26:20, \(x) paste0(LETTERS[x], LETTERS)))[1:80],
  sleep_quality = xbin,
  drank = drink,
  psolve = 55 + b_scond * sleep_good + bd * drink_yes + noise
) |> 
  mutate(sleep_quality = factor(sleep_quality, levels = c("Good", "Bad")))

# coef(lm(psolve~sleep_quality,eg3sleep[eg3sleep$drank=="Yes",]))
# 
# ggplot(eg3sleep, aes(x=sleep_quality,y=psolve,col=drank))+
#   geom_jitter(height=0,width=.2)+
#   stat_summary(geom="pointrange",position=position_nudge(x=.25))
# 
# ggplot(eg3sleep, aes(x=sleep_quality,y=psolve))+
#   geom_jitter(height=0,width=.2)+
#   stat_summary(geom="pointrange",position=position_nudge(x=.25))  
