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

#' Print a Dossier
#'
#' Takes an input of a specific id for a specific observation and returns the name and value for each variable in the dataset for that single observation. A different way of examining what your dataset looks like on a micro level.
#'
#' @param df a dataframe
#' @param id the id variable of your data frame
#' @param value the value of your individual dossier (must be in quotations)
#'
#' @export
dossier<-function(df, id, value, ...){
  id <- substitute(id)
  t(filter(df, !!id  == value))
}

#' Not In Pipe
#'
#" @export
`%notin%` <- function(x, y) {
  !(x %in% y)
}



#' Check if a variable has data extant
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


#' Check if a variable is Numeric
#'
#' Sometimes you have an unknown number of variables being piped into a function that only takes numeric vectors. This function exists to select only those which will work in the final function.
#'
#' works best with "select_if()" as in:
#' data %>% select_if(is_numeric)
#'
#'
#' @param x a data vector
#' @return a logical: TRUE or FALSE
#'
#' @export
is_numeric<-function(x) any(is.numeric(x))




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



#' Model Diagnostics
#'
#' Of unknown provenance--been in my files for years now.
#' Returns a model sumamry plus three graphs of model fit statistics/diagnostics
#' in base R.
#'
#' @param model the linear model
#'
#' @export
diagnostics<-function(model){
  #run model and print specific output
  model1<-model
  stats<-round(c(summary(model1)$fstatistic[c(1,3)],
                 summary(model1)$sigma,
                 summary(model1)$r.squared,
                 summary(model1)$adj.r.squared),3)
  names(stats)<-c("F","DF", "Sigma","Rsq","AdjRsq")
  l1<-list(round(summary(model1)$coefficients,3), stats)
  names(l1)<-c("Coefficients","Stats")
  print(l1)

  #run specific diagnostic tests
  par(mfrow=c(1,3))
  hist(model1$residuals, main="Histogram of residuals", xlab="")
  plot(model1, 1)
  plot(model1, 2)
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
  x<-read.table("clipboard",sep="\t",header=header,stringsAsFactors=TRUE,...)
  tibble::as_tibble(x)
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
  tidyr::crossing(trial = 1:n,
           flip = 1:m) %>%
    dplyr::mutate(heads = rbinom(n(),1,0.5)) %>%
    dplyr::group_by(trial) %>%
    dplyr::mutate(next_flip = lead(heads),
           hh = heads & next_flip,
           ht = heads & !next_flip) %>%
    dplyr::summarise(first_hh = which(hh)[1] + 1,
              first_ht = which(ht)[1] + 1) %>%
    dplyr::summarise(first_hh = mean(first_hh),
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
    dplyr::top_n(n) %>%
    dplyr::mutate(group := fct_reorder(!!group, n)) %>%
    ggplot2::ggplot(., aes_(y=quo(n))) +
    ggplot2::geom_bar(aes(group),stat = "identity") +
    ggplot2::coord_flip()
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

#' view correctly
#'
#' @export
view <- function(...){
  View(...)
}

#' Wrap text in ggplot2
#'
#' taken from twitter @geokaramanis but he attributes it to an anon stack overflow postl
#'
#' example: subtitle = wrapper("Really long text...", width = 80)
#'
#' @param x some text for a ggplot label.
#'
#' @return wrapped text for a ggplot label.
#'
#' @export
wrapper <- function(x, ...){
  paste(strwrap(x, ...), collapse = "\n")
}

e <- function(){
  fact<-function(n){
    if (n == 0){
      return(1)
    } else {
      return(n*fact(n - 1))
    }
  }
  calc_e<-function(i){
    if (i == 0) {
      return(1)
    } else {
      return( 1/ fact(i) + calc_e(i - 1))
    }
  }
  return(calc_e(50))
}
