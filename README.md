# mccrr
## A Personal R package

This package contains objects and functions that I have found via the #rstats community and some things I have developed myself. These are functions and objects I use on a regular basis and so have committed to a package for ease of access. Where possible, credit is given to the original poster.


## Name
The name derives from Yihui Xie's joke about how Hadley Wickham derives his r package names:
```r 
tidy_name = function(x) {
  x = tolower(substr(abbreviate(x), 1, 4))
  paste(c(x, rep('r', 5 - nchar(x))), collapse = '')
}
```
## Statar
The package currently contains some functions that are intended to bridge the code-switching problem when working in both Stata and R. `stata_summary` gives regression output in stata's format. Additional functions exist for common data manipulations that have different names in the two languages, e.g. `regress` for `lm()` and `browse` for `View()` (Also `view()` for `View()` in this package. Sometimes you just get confused and type the wrong thing. The package is here to help.


## to add:
https://multithreaded.stitchfix.com/blog/2017/06/15/beware-r-in-production/
