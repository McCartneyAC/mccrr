# tools and functions for regression discontinuity designs

# These may become a part of a package of tools associated with Dr. Miller's Causal Inference class at UVA


#' Center a Running Variable
#'
#' @param df a data frame
#' @param var the variable you wish to center
#' @param center the value around which you wish to center your variable (default is zero--does nothing)
#'
#' @return a data frame with the chosen variable centered
#'
#' @export
center <- function(df, var, center = 0) {
  # centers a running variable on user input center
  # first successful tidyeval function !!!
  user_var <- enquo(var)
  varname <- quo_name(user_var)
  center <- enquo(center)
  mutate(df, !!varname := (!!user_var-!!center))
}
