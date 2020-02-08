#' Calculate the Percent change in R^2
#'
#' Stolen directly from Steven Pollack.
#' https://github.com/stevenpollack/stat151a/blob/master/From_Lab/Feb-26-2014.R
#'
#'  @param baseModel a basic regression model
#'  @param extendedModel an extended version of that model (i.e. with an additional variable or more)
#'
#'  @return A percent increase of the extended model's efficiency over the base model
#'
#' @export
calcR2Increase <- function(baseModel, extendedModel) {
  extendedModelR2 <- var(extendedModel$fitted.values)/var(Duncan$prestige)
  baseModelR2 <- var(baseModel$fitted.values)/var(Duncan$prestige)

  # find the percent increase
  abs(baseModelR2 - extendedModelR2) / baseModelR2
}


#' Create an Added Variable Plot
#'
#' Adapted from Steven Pollack
#' https://github.com/stevenpollack/stat151a/blob/master/From_Lab/Feb-26-2014.R
#' (combines his data manipulation and graphing of av plots and removes formatting (can be added back in or adjusted as needed)
#'
#' @param partial the model containing only the output and the variable for which you wish to graph the av plot
#' @param extended the full model from which you are drawing the partial variable
#' @param se whether to graph the standard error around the regression line
#'
#' @return a ggplot2 graph that can be extended via other ggplot2 arguments
#'
#' @export
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
