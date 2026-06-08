# ---
# Lab settings
# ---

# Knitr settings
knitr::opts_chunk$set(
    echo = TRUE,
    message = FALSE, 
    warning = FALSE,
    fig.align = 'center',
    out.width = '100%'
)
options(digits=3)

# ggplot settings
library(ggplot2)
theme_set(
    theme_bw(base_size = 15)
)

# martin's handy printing function
.pp <- function(command,top=3,bottom=-3,l=FALSE) {
  t <- capture.output(eval(command))
  ln <- length(t)
  if (class(l)!='logical') {
    i=0
    for (n in l) {
      if (i>0) {
        cat("...",sep="\n")
      }
      i=i+1
      if (length(n)==1 && (i == 1 || i == length(l))) {
        if (n<0) {
          s <- n+1+ln
          n <- c(s:ln)
        } else {
          n <- c(1:n)
        }
      }
      if (!(0 %in% n)) {
        cat(t[n],sep="\n")
      }
    }
  } else {
    if (top != 0) {
      cat(t[1:top],sep="\n")
    }
    cat("...",sep="\n")
    if (bottom !=0) {
      bottom <- bottom+1+ln
      cat(t[bottom:ln],sep="\n")
    }
  }
}

.rround <- function(x,d=2,drop.zero=F) {
  y <- format(round(as.numeric(x),d),nsmall=d)
  if (drop.zero) {
    y <-  sub('^(-)?0\\.','\\1.',y)
  }
  y
}

