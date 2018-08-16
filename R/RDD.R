# tools and functions for regression discontinuity designs

# These may become a part of a package of tools associated with Dr. Miller's Causal Inference class at UVA

center_rv <- function(df, var, center = 0) {
  # centers a running variable on user input center
  # first successful tidyeval function !!!
  user_var <- enquo(var)
  varname <- quo_name(user_var)
  center <- enquo(center)
  mutate(df, !!varname := (!!user_var-!!center))
}