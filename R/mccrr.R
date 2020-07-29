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


#' Check if a variable is has data extant
#'
#' Sometimes for longitudinal data, variables exist but have not been entered yet. To ensure forward compatibility, this function checks to see if any data have yet been entered for a given variable.
#'
#' works best with "select_if()" as in:
#' data %>% select_if(is_extant)
#'
#'
#' @param x a data vector
#' @return a logical: TRUE or FALSE
#'
#' @export
is_extant <-function(x) any(!is.na(x))

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

#' Correlation
#'
#' A fixed correlation function; it should operate in a pipe and also defaults to
#' pairwise complete observations being true. Additional arguments to base `cor()`
#' are also available.
#'
#' @param df A data frame
#' @param x The x variable
#' @param y The y variable
#'
#' @export
correlate<-function(df, x, y, ...){
  x <- substitute(x)
  y <- substitute(y)
  cx<- df %>% select(!!x)
  cy<- df %>% select(!!y)
  return(cor(cx, cy, use = "pairwise.complete.obs", ...))
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

#' Solve a Quadratic Equation
#'
#' Just another practice function.
#' returns roots in complex notation regardless of root form
#'
#' @param a the first coefficient in standard form
#' @param b the second coefficient
#' @param c the third coefficient
#'
#' @return two roots either real or imaginary. They will be in complex form notation. Roots are real if the imaginary term is 0i. If one term is 0 + 0i then there is only one root.
#'
#' @export
solve_quadratic<-function(a,b,c){
  det<-sqrt(as.complex(b^2-4*a*c))
  numerator<-(-b+det)
  denom<-(2*a)
  pos <- numerator/denom
  numerator<-(-b-det)
  neg <- numerator/denom
  result<-c(pos,neg)
  return(result)
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



#' Description of All variables in a Dataset
#'
#' cleans the output of `psych::describe()` and passes it into
#' a `gt::gt()` object to make an HTML output
#'
#' @param data your data
#' @param group optional: your grouping variable
#'
#' @return an HTML object in the viewer pane (or in a shiny app or markdown document)
#'
#' @export
description<-function(data, group = NULL, fast = TRUE, ...) {
  grp<-paste0(deparse(substitute(group)))
  #print(grp)


  if(is.null(group)) {
    data %>%
      psych::describe(fast = fast, ...) %>%
      tibble::rownames_to_column() %>%
      dplyr::select(-c(vars)) %>%
      dplyr::mutate(dplyr::across(is.numeric, round, 2)) %>%
      gt::gt() %>%
      gt::tab_options(
        column_labels.font.size = "small",
        table.font.size = "small",
        row_group.font.size = "small",
        data_row.padding = px(3)
      ) %>%
      gt::tab_header(
        title = paste0("Data Description")
      )
  } else {
    data %>%
      select_if(is.numeric) %>%
      psych::describeBy(group = group, fast = fast, mat= TRUE, ...) %>%
      tibble::rownames_to_column() %>%
      dplyr::select(-c(item, vars)) %>%
      dplyr::mutate(dplyr::across(is.numeric, round, 2)) %>%
      dplyr::arrange(group1) %>%
      dplyr::group_by(group1) %>%
      gt::gt() %>%
      gt::tab_options(
        column_labels.font.size = "small",
        table.font.size = "small",
        row_group.font.size = "small",
        data_row.padding = px(3))
  } %>%
    gt::tab_header(
      title = paste0("Data Description") ,
      subtitle = paste0("Grouped by: ",  grp )
    )
}
