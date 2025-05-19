rm(list = ls())

library(tidyverse)

ff = list.files(".", ".qmd", full.names=T)
ff

getlib <-function(f){
    knitr::purl(f,output="tmp.R")
    sc=readLines("tmp.R")
    sc[grepl("library|::",sc)]
}

all_libs = lapply(ff, getlib) |> unlist() |> unique()
all_libs

all_libs = gsub("::.*", "\\1", all_libs)

all_libs = all_libs |> str_remove_all("#")
all_libs

all_libs = all_libs |> str_trim(side = "left")

all_libs = gsub("(\\)).*", "\\1", all_libs)

all_libs = str_replace(all_libs, "library\\(", "")
all_libs = str_replace(all_libs, "\\)", "")
all_libs = unique(all_libs)
all_libs

all_libs = str_subset(all_libs, "%>%|\\(", negate = TRUE)

install.packages(all_libs)
