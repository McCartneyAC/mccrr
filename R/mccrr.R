# from Jenny Bryan @JennyBryan
# https://twitter.com/JennyBryan/status/980978609794895872
vlookup<-function(this,data,key,value){
  require(tidyverse)
  # this is a list of values
  # data is a dataset to match it to
  # key is the variable in the data to match on
  # value is the value applied to "this"
  m<-match(this, data[[key]])
  data[[value]][m]
}


# plot a simple linear model
# https://twitter.com/katiejolly6/status/960653271080865798
# http://katiejolly.io/blog/2018-02-05/aes-string
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


# from https://www.r-bloggers.com/copying-data-from-excel-to-r-and-back/
# adapted for tibbles
paste_data <- function(header=TRUE,...) {
  require(tibble)
  x<-read.table("clipboard",sep="\t",header=header,stringsAsFactors=TRUE,...)
  as.tibble(x)
}
copy_data <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}


# frequent color palettes
mexico_city <- c("#E12100", "#CCB200", "#114511", "#9f86cb", "#000000", "#AAAAAA")
uvapal <- c("#E57200","#232D4B", "#007681","#F2CD00","#692A7E", "#84BD00","#A5ACAF", "#5C7F92","#857363","#CAC0B6") 

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


# -------------------------------------------------
# twitter post deleted, author lost to history
stata_summary <-
  function(
    x,
    ...
  ) {
    # summarize
    mod <- x
    x <- summary(x)
    
    # find outcome variable
    outcome <- as.character(attr(terms(x), "variables")[[2L]])
    
    # summary statistics
    ## total sum of squares
    sst <- sum((mod[["model"]][[outcome]] - mean(mod[["model"]][[outcome]], na.rm = TRUE))^2)
    ## sum of squared residuals
    ssr <- sum(residuals(x)^2)
    ## model sum of squares
    ssm <- sst - ssr
    ## number of observations
    n_obs <- length(residuals(x))
    ## number of RHS variables
    n_var <- length(attr(terms(x), "term.labels"))
    ## f-statistic (f, df1, df2)
    fstat <- x$fstatistic
    ## f-statistic p-value
    fstat_p <- pf(x$fstatistic[1L], x$fstatistic[2L], x$fstatistic[3L], lower.tail = FALSE)
    ## r-squared
    rsq <- x$r.squared
    ## adjusted r-squared
    arsq <- x$adj.r.squared
    ## root mse
    sigma <- x$sigma
    
    # get coefficients
    coefficients <- coef(x)
    ## extract constant, reshape and rename
    const_row <- which(rownames(coefficients) == "(Intercept)")
    coefficients <- rbind(coefficients[-const_row,], "_cons" = coefficients[const_row,])
    ## get confidence intervals
    coefficients <- cbind(
      coefficients,
      CI_lower = coefficients[,1L] - (qt(0.975, n_obs-n_var-1L) * coefficients[,2L]),
      CI_upper = coefficients[,1L] + (qt(0.975, n_obs-n_var-1L) * coefficients[,2L])
    )
    ## colnames
    colnames(coefficients) <- c("Coef.", "Std. Err.", "t", "P>|t|", "", "[95% Conf. Interval]")
    
    ## format columns
    coef_character <- coefficients
    storage.mode(coef_character) <- "character"
    print_width <- function(vec, digits = 7, drop_zero = FALSE) {
      max_digits <- max(nchar(as.character(round(vec, 0))), na.rm = TRUE)
      neg <- (vec < 0)
      lead_zero <- (abs(vec) < 1)
      if (isTRUE(drop_zero)) {
        vec <- sub("^0", "", ifelse(abs(vec) > 1, 
                                    sprintf(paste0("%0.", max(c(2L, digits-max_digits)), "f"), abs(vec)),
                                    sprintf(paste0("%0.", digits, "f"), abs(vec))
        ))
        vec <- paste0(ifelse(neg, "-", " "), vec)
        #vec <- paste0(ifelse(lead_zero, " ", ""), vec)
      } else {
        vec <- ifelse(abs(vec) > 1,
                      sprintf(paste0("%0.", max(c(2L, digits-max_digits)), "f"), abs(vec)),
                      sprintf(paste0("%0.", digits, "f"), abs(vec))
        )
        #vec <- formatC(abs(vec), digits = 7, width = 7)
        vec <- paste0(ifelse(neg, " -", "  "), vec)
      }
      vec
    }
    ### coefficients (7 digits)
    coef_character[,1L] <- print_width(coefficients[,1L], 7, drop_zero = TRUE)
    ### standard errors (7 digits)
    coef_character[,2L] <- print_width(coefficients[,2L], 7, drop_zero = TRUE)
    ### t-statistics (2 digits)
    coef_character[,3L] <- print_width(coefficients[,3L], 2)
    ### t-statistics (3 digits)
    coef_character[,4L] <- print_width(coefficients[,4L], 3)
    ### CI bounds (7 digits)
    coef_character[,5L] <- print_width(coefficients[,5L], 7, drop_zero = TRUE)
    coef_character[,6L] <- print_width(coefficients[,6L], 7, drop_zero = TRUE)
    rownames(coef_character) <- formatC(rownames(coef_character))
    
    # max variable name length
    max_nchar <- max(c(nchar(rownames(coefficients)), na.rm = TRUE), nchar("Residual"), na.rm = TRUE)
    
    
    # print
    nchar_n_obs <- max(c(nchar(as.character(n_obs)), 9L))
    ## summary tables
    fstat_padded <- formatC(paste0("F(", fstat[2L], ", ", fstat[3L], ")"), width = 15, flag = "-")
    ### ANOVA table
    out_mat1 <- matrix(NA_character_, nrow = 6, ncol = 5)
    out_mat1[1L,] <- c("      Source", " | ", "       SS", "          df", "       MS   ")
    out_mat1[2L,] <- c("------------", "-+-", "---------", "------------", "------------")
    out_mat1[5L,] <- c("------------", "-+-", "---------", "------------", "------------")
    out_mat1[c(3,4,6),1L] <- c("       Model", "    Residual", "       Total")
    out_mat1[c(3,4,6),2L] <- rep(" | ", 3)
    out_mat1[c(3,4,6),3L] <- formatC(c(sprintf("%0.5f", ssm), sprintf("%0.5f", ssr), sprintf("%0.5f", sst)), width = 12)
    out_mat1[c(3,4,6),4L] <- formatC(c(fstat[2L], fstat[3L], fstat[2L] + fstat[3L]), width = 9)
    out_mat1[c(3,4,6),5L] <- formatC(c(ssm/fstat[2L], ssr/fstat[3L], sst/(fstat[2L] + fstat[3L])), width = 12, digits = 9)
    ### Summary statistics
    out_mat2 <- matrix(NA_character_, nrow = 6, ncol = 3)
    out_mat2[,1L] <- c("Number of obs  ", fstat_padded, "Prob > F       ", "R-squared      ", "Adj R-squared  ", "Root MSE       ")
    out_mat2[,2L] <- rep(" = ", 6)
    out_mat2[,3L] <- formatC(c(n_obs, sprintf("%0.4f", c(fstat[1L], fstat_p, rsq, arsq, sigma))), width = nchar_n_obs)
    
    ## print
    out <- apply(cbind(out_mat1, rep("  ", 6), out_mat2), 1L, paste0, collapse = "")
    for(i in seq_along(out)) {
      cat(out[i], "\n")
    }
    rm(out)
    cat("\n")
    
    ## coefficient table
    cat(rep("-", (max_nchar + 69L)), "\n", sep = "")
    cat(paste0(formatC(outcome, width = max_nchar), " |      Coef.   Std. Err.        t     P>|t|     [95% Conf. Interval]"), "\n")
    cat(rep("-", (max_nchar + 69L)), "\n", sep = "")
    for (i in seq_len(nrow(coefficients))) {
      cat(paste0(rownames(coef_character)[i], " |  ", 
                 coef_character[i,1L], "   ",
                 coef_character[i,2L], "   ",
                 coef_character[i,3L], "   ",
                 coef_character[i,4L], "   ",
                 coef_character[i,5L], "    ",
                 coef_character[i,6L], "   "),
          "\n")
    }
    cat(rep("-", (max_nchar + 69L)), "\n", sep = "")
    cat("\n")
    # return invisibly
    invisible(x)
  }


# ----------------------------------------------------------------------
# https://twitter.com/drob/status/1008409373423611904
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
