


# martin's
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
