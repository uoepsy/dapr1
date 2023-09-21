## basically, economist theme with big legends etc.

library(ggthemes)

theme_presentation<- function(base_size = 20, base_family = "") {
  # Start with theme_economist
  tr <- function(prop) {
    round(base_size*prop)
  }
  
  theme_economist(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # strip.text.x = element_text(size = tr(64)),
      # strip.text.y = element_text(size = tr(64)),
      axis.text.x = element_text(size=tr(1),
                                 margin=margin(tr(.7),0,0,0)),
      axis.text.y = element_text(size=tr(1)),
      axis.title.x= element_text(size=tr(1.25),face='italic',
                                 margin=margin(tr(.1),0,0,0)),
      axis.title.y= element_text(size=tr(1.25),angle=90,face='italic',
                                 margin=margin(0,tr(.1),0,0)),
      plot.title =element_text(size=tr(1)),
      legend.text=element_text(size=tr(.8)),
      legend.title=element_text(size=tr(.8),face='italic')
  )
}

update_geom_defaults("line",list(size=2))
theme_set(theme_presentation())
