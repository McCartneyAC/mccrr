# stolen directly from Steven Pollack
# https://github.com/stevenpollack/stat151a/blob/master/From_Lab/Feb-26-2014.R
# calculate R^2 of the two models
calcR2Increase <- function(baseModel, extendedModel) {
  extendedModelR2 <- var(extendedModel$fitted.values)/var(Duncan$prestige)
  baseModelR2 <- var(baseModel$fitted.values)/var(Duncan$prestige)
  
  # find the percent increase
  abs(baseModelR2 - extendedModelR2) / baseModelR2 
}


# Adapted from Steven Pollack
# https://github.com/stevenpollack/stat151a/blob/master/From_Lab/Feb-26-2014.R
# (combines his data manipulation and graphing of av plots
# and removes formatting (can be added back in or adjusted as needed)
gg_added_var <- function(partial, extended, se = TRUE) {
  # In a multiple regression, the added variable plot for a predictor X, say, 
  # is the plot showing the residual of Y against all predictors except X against the 
  # residual of X on all predictors except X, of course.
  require(ggplot2)
  partial_residuals <- resid(partial)
  full_residuals <- resid(extended)
  if (se) {
    avPlot <- ggplot(
      data = data.frame(x = partial_residuals, y = full_residuals),
      aes(x = partial_residuals, y = full_residuals)
    ) +
      geom_point() +
      stat_smooth(method = "lm")
  } else {
    avPlot <- ggplot(
      data = data.frame(x = partial_residuals, y = full_residuals),
      aes(x = partial_residuals, y = full_residuals)
    ) +
      geom_point() +
      stat_smooth(method = "lm", se = FALSE)
  }
  return(avPlot)
}
