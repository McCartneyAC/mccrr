#tmp <- capture.output(stem(iris$Petal.Length))
#stemdf = data.frame(tmp, rr=1:length(tmp))
#ggplot(stemdf)+ geom_text(aes(x=rr, y=0, label=tmp), hjust=0) +
#    coord_flip()+ theme_classic() +
#    scale_x_discrete(breaks=NULL)+
#    scale_y_discrete(breaks=NULL, limits=c(0,1))+
#    theme(axis.text = element_blank(),
#        axis.title = element_blank(),
#        axis.ticks=element_blank(),
#        panel.grid=element_blank(),
#        axis.line=element_blank())
#https://stackoverflow.com/questions/26532564/how-to-output-a-stem-and-leaf-plot-as-a-plot


# conversion function from above. 
stemleaf_to_df<-function(df) {
  tmp<-capture.output(stem(df))
  dat<-data.frame(tmp, rr = 1:length(tmp))
  return(dat)
}

# theme elements from above. 
theme_stemleaf <- ggplot2::theme(axis.text = ggplot2::element_blank(),
                        axis.title = ggplot2::element_blank(),
                        axis.ticks = ggplot2::element_blank(),
                        panel.grid = ggplot2::element_blank(),
                        axis.line = ggplot2::element_blank()) + 
                        ggplot2::coord_flip()+ 
                        # theme_classic() + or just use theme_void() + 
                        ggplot2::scale_x_discrete(breaks=NULL) + # may need to remove these
                        ggplot2::scale_y_discrete(breaks=NULL, limits=c(0,1))


## ggproto part
StatStemleaf <- ggplot2::ggproto(
  "stemleaf", 
  Stat,
  required_aes = c("x"), 
  
  compute_group = function(data, scales){
    tmp<-capture.output(stem(data$x))
    data.frame(y = tmp, x = 1:length(tmp), label = tmp)
  }
)

# the stat itself. Somewhere, it's mapping something continuous to discrete. But what? 
stat_stemleaf <-  function(mapping = NULL , # aes(x = x, y = 0, label = tmp)
                           data = NULL,
                           geom = "text",
                           position = "identity",
                           na.rm = TRUE,
                           show.legend = NA,
                           inherit.aes = TRUE, ...) {
  ggplot2::layer(
      stat = StatStemleaf,
      data = data,
      mapping = mapping,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, hjust = 0,  ...)
    )
  }                        
                        
