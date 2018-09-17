theme_andrew <-function(fam = "Raleway"){
  # families <- c("Raleway","Dusha V5", "xkcd", "Fira Code", "Space Mono", "Homemade Apple", "Ink Free")
  theme(
    text = element_text(family = fam), 
    panel.background = element_blank(), 
    axis.ticks = element_line(colour = "gray"),
    panel.grid.major = element_line(colour = "grey90", size = 0.2), 
    panel.grid.minor = element_line(colour = "grey98", size = 0.5), 
  )
}


theme_textbook <- function() {
  theme_light() + 
    theme(text = element_text(family = "SimSun-ExtB"))
}
