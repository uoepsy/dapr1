# ---
# Lab settings
# ---

# Dropdowns
source('assets/dropdowns.R')

# # Knitr settings
# knitr::opts_chunk$set(
#     echo = TRUE,
#     message = FALSE, 
#     warning = FALSE,
#     fig.align = 'center',
#     out.width = '80%',
#     comment = ''
# )

knitr::opts_chunk$set(echo = TRUE)

# ggplot settings
library(ggplot2)
theme_set(
    theme_bw(base_size = 15)
)

# kable settings
options(knitr.table.format = "simple")
