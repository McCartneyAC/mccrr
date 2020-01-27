
theme_textbook <- function() {
  ggplot2::theme(text = ggplot2::element_text(family = "SimSun-ExtB")) +
    ggplot2::theme_light()
}


theme_andrew <-function(fam = "Raleway"){
 # families <- c("Raleway","Dusha V5", "xkcd", "Fira Code", "Space Mono", "Homemade Apple", "Ink Free",
 # "Montez", "Cinzel Decorative", "Kristi", "Sofia", "Love Ya Like A Sister", "FrederickatheGreat", "Special Elite")
 ggplot2::theme(
   text = ggplot2::element_text(family = fam),
   panel.background = ggplot2::element_blank(),
   axis.ticks = ggplot2::element_line(colour = "gray"),
   panel.grid.major = ggplot2::element_line(colour = "grey90", size = 0.2),
   panel.grid.minor = ggplot2::element_line(colour = "grey98", size = 0.5)
 )
}


theme_textbook <- list(
 ggplot2::theme_light(),
 ggplot2::theme(text = ggplot2::element_text(family = "SimSun-ExtB"))
)

theme_looseleaf<- list(
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white")) , 
  ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "navy")) , 
  ggplot2::theme(panel.grid.minor.y = ggplot2::element_line(colour = "navy")) ,
  ggplot2::theme(panel.grid.major.x = NULL),
  ggplot2::theme(panel.grid.minor.x = NULL),
  ggplot2::geom_vline(xintercept = 0, color = "red") #should be something like "range"
)



pal_inova <- function(palette = c("inova"), alpha = 1) {
 palette <- match.arg(palette)
 if (alpha > 1L | alpha <= 0L) stop("alpha must be in (0, 1]")
 raw_cols <- inova
 raw_cols_rgb <- col2rgb(raw_cols)
 alpha_cols <- rgb(
   raw_cols_rgb[1L, ], raw_cols_rgb[2L, ], raw_cols_rgb[3L, ],
   alpha = alpha * 255L, names = names(raw_cols),
   maxColorValue = 255L
 )
 scales::manual_pal(unname(alpha_cols))
}

#' Scale Colors as inova healthsystems
#'
#' @export scale_color_inova
scale_color_inova <- function(palette = c("inova"), alpha = 1, ...) {
 palette <- match.arg(palette)
 ggplot2::discrete_scale("colour", "inova", pal_inova(palette, alpha), ...)
}
scale_colour_inova <- scale_color_inova


#' Scale fill as inova healthsystems
#'
#' @export scale_fill_inova
scale_fill_inova <- function(palette = c("inova"), alpha = 1, ...) {
 palette <- match.arg(palette)
 ggplot2::discrete_scale("fill", "inova", pal_inova(palette, alpha), ...)
}


# # frequent color palettes
mexico_city <- c("#E12100", "#CCB200", "#114511", "#9f86cb", "#000000", "#AAAAAA")
uvapal <- c("#E57200","#232D4B", "#007681","#F2CD00","#692A7E", "#84BD00","#A5ACAF", "#5C7F92","#857363","#CAC0B6")
acled <- c("#274f5c", "#2a788d", "#ff8f2b","#adcfee","#ffc38a","#6ba5d4","#d0671f","#1d1d1d","#979797","#d5d5d5")
inova<-c("#004b8d", "#d52b1e", "#6caddf", "#4d4f53", "#a5a5a9")
