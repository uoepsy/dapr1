
library(tidyverse)
colours <- c("#6bcded", "#b38ed2", "#85a6ea", "#eda46f", "#d8d768", "#77bd9d")
df <- data.frame(x = 1:1000, y = 0)
df_box <- data.frame(x = seq(1,length(df$x), length.out = 6), y = 0, 
                     label = c("dapR 1", "dapR 2", "dapR 3", "Mini\nDissertation", "Disseration", "Life"))

df %>% ggplot(aes(x, y, colour = x)) + 
  geom_point(size = 5) +
  geom_tile(data = df_box, fill = "#fdfdfd", size = 1, height = 1.25, width = 170) +
  annotate("text", x = df_box$x, y = 0, label = df_box$label, 
           colour = c("#6bcded", "#b38ed2", "#85a6ea", "#eda46f", "#d8d768", "#77bd9d"), size = 5) +
  scale_color_gradientn(colors = rep(colours, each = 3)[-c(1, 18)], guide = F) + ylim(-3, 3) +
  theme_void() + theme(panel.background = element_rect(fill = "#fdfdfd"))
ggsave("timeline1.png")
dev.off()

par(mar = c(0, 0, 0, 0), oma = rep(0, 4), xpd=TRUE, ljoin = 1, lmitre = 60)
df <- data.frame(x = seq(0, 12.5, length.out = 50), y = 0)

end_col <- colorRampPalette(colours[1:2])(4)[3]

png("tile01.png", width = 2400, height = 1100, bg = "#fdfdfd")
plot(NULL, xlim = c(0, 100), ylim = c(-2.9, 2.9),
     bty=NULL, pch = 15, cex = 2, axes = F, xlab = "", ylab = "")
for (i in seq(-.6, .4, by = .1))
  points(100 - df$x, df$y + i, col = colorRampPalette(c(end_col, colours[1]))(nrow(df)), pch = 15, cex = 5)

rect(13, -3, 87, 3.6, border = colours[1], lwd = 12)
rect(12.7, 2, 87.25, 3.7, col = colours[1], border = NA)
dev.off()

for(i in 2:5) {
  start_col <- colorRampPalette(colours[c(i-1, i)])(3)[2]
  end_col <- colorRampPalette(colours[c(i, i+1)])(4)[3]
  png(paste0("tile0", i, ".png"), width = 2400, height = 1100, bg = "#fdfdfd")
  plot(NULL, xlim = c(0, 100), ylim = c(-2.9, 2.9),
       bty=NULL, pch = 15, cex = 2, axes = F, xlab = "", ylab = "")
  for (j in seq(-.6, .4, by = .1)) {
    points(df$x, df$y + j, col = colorRampPalette(c(start_col, colours[i]))(nrow(df)), pch = 15, cex = 5)
    points(100 - df$x, df$y + j, col = colorRampPalette(c(end_col, colours[i]))(nrow(df)), pch = 15, cex = 5)
  }
  rect(13, -3, 87, 3.6, border = colours[i], lwd = 12)
  rect(12.7, 2, 87.25, 3.7, col = colours[i], border = NA)
  dev.off()
}

png("tile06.png", width = 2400, height = 1100, bg = "#fdfdfd")
plot(NULL, xlim = c(0, 100), ylim = c(-2.9, 2.9),
     bty=NULL, pch = 15, cex = 2, axes = F, xlab = "", ylab = "")
for (j in seq(-.6, .4, by = .1))
  points(df$x, df$y + j, col = colorRampPalette(c(colorRampPalette(colours[c(5, 6)])(3)[2], colours[6]))(nrow(df)), pch = 15, cex = 5)

rect(13, -3, 87, 3.6, border = colours[6], lwd = 12)
rect(12.7, 2, 87.25, 3.7, col = colours[6], border = NA)
dev.off()

