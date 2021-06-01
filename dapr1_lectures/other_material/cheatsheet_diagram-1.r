
library(tidyverse)
df <- as.data.frame(expand.grid(0:2, 1:7))
names(df) <- c("x", "y")
df$colour <- c(rep(1:3, each = 6), 4, 4, 4)
df$colour[df$x > 0] <- 9 
df$colour[df$y == 7] <- 4
df2 <- as.data.frame(expand.grid(10:12, 2:5))
names(df2) <- c("x", "y")
df2$colour <- rep(1:4, each = 3)
df2$colour[df2$x > 10] <- df2$colour[df2$x > 10] + 4
df_arrow <- data.frame(x = c(3.5, 8.5), y = 4)
translate_y <- c(rep(c(-.5, 0, .5), each = 6), rep(.5, 3))
df %>% ggplot() +
  geom_rect(aes(xmin= x, xmax = x + .9, ymin = y, ymax = y + .9,
                fill = factor(colour))) +
  geom_rect(aes(xmin= x + 5, xmax = x + 5.9,
                ymin = y + translate_y, ymax = y + .9 + translate_y,
                fill = factor(colour))) +
  geom_rect(data = df2, aes(xmin= x, xmax = x + .9, ymin = y, ymax = y + .9,
                fill = factor(colour))) +
  geom_segment(data = df_arrow,
               aes(x, y, xend = x + 1, yend = y, group = x),
               colour = "#797b81", size = 1, linejoin = "mitre",
               arrow = arrow(length = unit(10, "points"),
                             type = "closed", angle = 20)) +
  xlim(c(0, 13)) + ylim(c(0, 9)) + coord_fixed() + scale_fill_manual(values = c("#d6a34d", "#f2c673", "#f7dca7", "#aeb0af", "#4f79a6", "#83a9d2", "#bedafa", "#244b7b", "#d8d9da"), guide = F) +
  theme_void()

