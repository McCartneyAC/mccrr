#' Vlookup a value
#'
#' from Jenny Bryan @JennyBryan
#' https://twitter.com/JennyBryan/status/980978609794895872
#'
#' @param this a list of values
#' @param data a dataset to match it to
#' @param key the variable in the data to match on
#' @param value  the value applied to "this"
#'
#' @export
vlookup<-function(this,data,key,value){
  m<-match(this, data[[key]])
  data[[value]][m]
}


#' Plot a simple linear model
#'
#' https://twitter.com/katiejolly6/status/960653271080865798
#' http://katiejolly.io/blog/2018-02-05/aes-string
#'
#' @param mod the linear model
#' @param explanatory the x variable
#' @param response the y variable
#'
#' @export
plot_model <- function(mod, explanatory, response, .fitted = ".fitted") {
  augment(mod) %>%
    ggplot() +
    geom_point(aes_string(x = explanatory, y = response), color = "#2CA58D") +
    geom_line(aes_string(x = explanatory, y = .fitted), color = "#033F63") +
    theme_solarized() +
    theme(axis.title = element_text())
}

# joke package name generator from Yihui Xie @xieyihui
tidy_name <- function(x) {
  x = tolower(substr(abbreviate(x), 1, 4))
  paste(c(x, rep('r', 5 - nchar(x))), collapse = '')
}

down_name <- function(x) tolower(paste0(gsub('\\s+', '', x), 'down'))

#' Paste data from an Excel Spreadsheet
#'
#' from https://www.r-bloggers.com/copying-data-from-excel-to-r-and-back/
#' adapted for tibbles
#'
#' @return a tibble of the pasted data
#'
#' @export
paste_data <- function(header=TRUE,...) {
  require(tibble)
  x<-read.table("clipboard",sep="\t",header=header,stringsAsFactors=TRUE,...)
  as_tibble(x)
}
# paste_data <- function (...) {
#     readr::read_tsv(readr::clipboard(), ...)
#  }


#' copy a dataframe to paste outward
#'
#' does not currently work
copy_data <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}


# frequent color palettes
#' @export
mexico_city <- c("#E12100", "#CCB200", "#114511", "#9f86cb", "#000000", "#AAAAAA")
#' @export
uvapal <- c("#E57200","#232D4B", "#007681","#F2CD00","#692A7E", "#84BD00","#A5ACAF", "#5C7F92","#857363","#CAC0B6")
#' @export
acled <- c("#274f5c", "#2a788d", "#ff8f2b","#adcfee","#ffc38a","#6ba5d4","#d0671f","#1d1d1d","#979797","#d5d5d5")


#' Print a palette of colors
#'
#' Maybe stolen from Karthik Ram?
#'
#' @param name a color palette
#' @param n how many colors to select
#' @param type whether to print the colors as discrete or continuous
#'
#' @export
palprint<- function(name, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)

  pal <- name
  if (is.null(pal))
    stop("Palette not found.")

  if (missing(n)) {
    n <- length(pal)
  }

  if (type == "discrete" && n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }

  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = "")
}



#' Flip a coin a few times
#'
#' from drob's twitter
#' https://twitter.com/drob/status/1008409373423611904
#'
#' @param n how many trials to run
#' @param m how many coinflips per trial
#'
#' @export
coinflips<-function(n = 10000, m = 100){
  require(tidyverse)
  crossing(trial = 1:n,
           flip = 1:m) %>%
    mutate(heads = rbinom(n(),1,0.5)) %>%
    group_by(trial) %>%
    mutate(next_flip = lead(heads),
           hh = heads & next_flip,
           ht = heads & !next_flip) %>%
    summarize(first_hh = which(hh)[1] + 1,
              first_ht = which(ht)[1] + 1) %>%
    summarize(first_hh = mean(first_hh),
              first_ht = mean(first_ht))
}

#------------------------------------------------------------
# for persistent data files via shiny app:
save_data_flatfile <-function(data) {
  data <-t(data)
  file_name <- paste0(paste(get_time_human(), digest(data,
                                                     algo = "md5"), sep = "_"), ".csv")
  write.csv(x = data, file = file.path(results_dir, file_name),
            row.names = FALSE, quote = FALSE)
}

load_data_flatfile <- function(){
  files <-list.files(file.path(results_dir), full.names = TRUE)
  data >= lapply(files, read.csv, stringsAsFactors = FALSE) %>%
    do.call(rbind, .)
  data
}

#' Plot Factors by Frequency
#'
#' https://stackoverflow.com/questions/46862482/plot-a-descending-frequency-bar-chart-using-a-custom-function-with-ggplot2-dply
#'
#' @param data a data frame
#' @param group a variable of factors
#' @param n how many to print into the ggplot graph
#'
#' @return a ggplot graph object
#'
#' @export
plot_freq <- function(data, group,  n=10){
  group <- enquo(group)
  data %>%
    count(!!group) %>%
    top_n(n) %>%
    mutate(group := fct_reorder(!!group, n)) %>%
    ggplot(., aes_(y=quo(n))) +
    geom_bar(aes(group),stat = "identity") +
    coord_flip()
}

#' Summarize a Regression
#'
#' Reorders summarize(lm()) to allow it to be the last verb of a pipe and operate in one step. 
#'
#' @param df a data frame
#'
#' @return A regression summary
#'
#' @export
regress <- function(df, ...) {
  summary(
    lm(data = df, ...)
  )
}

# valentine's day
heart<-function(){
  dat<- data.frame(t=seq(0, 2*pi, by=0.1) ) 
  xhrt <- function(t) 16*sin(t)^3 
  yhrt <- function(t) 13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t) 
  dat$y =yhrt(dat$t) 
  dat$x=xhrt(dat$t) 
  plot(y ~ x, data=dat, type="l", bty="n", xaxt="n", yaxt="n", ann=FALSE) 
  with(dat, polygon(x,y, col="hotpink")) 
  points(c(10,-10, -15, 15), c(-10, -10, 10, 10), pch=169, font=5)
}

#view correctly
view <- function(...){
  View(...)
}
