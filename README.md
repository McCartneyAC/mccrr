# mccrr
## A Personal R package

This package contains objects and functions that I have found via the #rstats community and some things I have developed myself. These are functions and objects I use on a regular basis and so have committed to a package for ease of access. Where possible, credit is given to the original poster.

The name derives from Yihui Xie's joke about how Hadley Wickham derives his r package names:
```r 
tidy_name = function(x) {
  x = tolower(substr(abbreviate(x), 1, 4))
  paste(c(x, rep('r', 5 - nchar(x))), collapse = '')
}
```
