library(tidyverse)

fun1 <- function(x) 10 + (4*x)
fun2 <- function(x) 10 - (4*x)
fun3 <- function(x) 5 + x^2
fun4 <- function(x) 5 + log(x)
fun5 <- function(x) x

ggplot(data = data.frame(x=0), aes(x=x)) +
  stat_function(fun = fun1) +
  xlim(0,10) +
  ylim(0,50) +
  ylab("f(x)")
ggsave("C:/Teaching/function1.png", scale = 0.25)

ggplot(data = data.frame(x=0), aes(x=x)) +
  stat_function(fun = fun2) +
  xlim(0,10) +
  ylim(-25,25) +
  ylab("f(x)")
ggsave("C:/Teaching/function2.png", scale = 0.25)

ggplot(data = data.frame(x=0), aes(x=x)) +
  stat_function(fun = fun3) +
  xlim(0,10) +
  ylim(0,50) +
  ylab("f(x)")
ggsave("C:/Teaching/function3.png", scale = 0.25)

ggplot(data = data.frame(x=0), aes(x=x)) +
  stat_function(fun = fun4) +
  xlim(0,10) +
  ylim(0,10) +
  ylab("f(x)")
ggsave("C:/Teaching/function4.png", scale = 0.25)

ggplot(data = data.frame(x=0), aes(x=x)) +
  stat_function(fun = fun5) +
  xlim(0,10) +
  ylim(0,10) +
  ylab("f(x)")
ggsave("C:/Teaching/function5.png", scale = 0.25)
