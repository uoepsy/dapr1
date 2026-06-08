

blend_colors <- function(x, y, alpha = 0.5) {
  x <- colorspace::hex2RGB(x)
  y <- colorspace::hex2RGB(y)
  z <- colorspace::mixcolor(alpha, x, y)
  colorspace::hex(z)
}
color_blender <-
  function(x, y)
    function(alpha = 0.5)
      blend_colors(x, y, alpha)
theme_quarto <- function (text_color = color_text,
                          background_color = color_bg,
                          text_font_size = 30,
                          accent_color = color_base,
                          title_font_size = 30) {
  blend <- color_blender(text_color, background_color)
  ggplot2::theme(
    line = ggplot2::element_line(color = blend(0.2)),
    rect = ggplot2::element_rect(fill = background_color),
    title = ggplot2::element_text(
      color = accent_color,
      size = title_font_size
    ),
    plot.background = ggplot2::element_rect(fill = background_color,
                                            color = background_color),
    panel.background = ggplot2::element_rect(fill = background_color,
                                             color = background_color),
    panel.grid.major = ggplot2::element_line(color = blend(0.8),
                                             inherit.blank = TRUE),
    panel.grid.minor = ggplot2::element_line(color = blend(0.9),
                                             inherit.blank = TRUE),
    axis.title = ggplot2::element_text(size = title_font_size * 0.8),
    axis.ticks = ggplot2::element_line(color = blend(0.8)),
    axis.text = ggplot2::element_text(color = blend(0.4), size = title_font_size * 0.7),
    legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
    legend.text = ggplot2::element_text(size = title_font_size * 0.8),
    plot.caption = ggplot2::element_text(size = text_font_size * 0.8,
                                         color = blend(0.3)),
    strip.text.x = element_text(size = 16)
  )
}

update_geom_defaults("line",list(size=2))

knitr::opts_chunk$set(dev.args = list(bg="transparent"))

options(digits=3,scipen=3)


knitr::opts_chunk$set(
  #fig.asp=.8,
  fig.align='center',
  #tidy=TRUE,
  tidy.opts=list(width.cutoff=70, arrow=TRUE, indent=2, args.newline=TRUE)
)
knitr::knit_hooks$set(crop = knitr::hook_pdfcrop)
color_base <- "#0F4C81"
color_text <- '#000000'
color_bg <- '#ffffff'
theme_set(theme_quarto())
