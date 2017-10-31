#==============================================================================#
#                                 bivariate                                    #
#==============================================================================#
#' bivariate
#'
#' \code{bivariate} Performs bivariate analysis of two categorical variables.
#' The analysis includes a chi-square test of association and difference in
#' proportion tests for each level of the categorical explanatory variable,
#' ordered by proportion.
#'
#' @param data Data frame containing response and explanatory variable
#' @param y Character string description for the response variable
#' @param x Character string description of the explanatory variable
#' @param title Character string containing the plot title
#' @param success Character string for the factor variable considered the success factor
#' @param conf Numeric between 0 and 1 for the designated confidence level
#' @param alpha Numeric between 0 and 1 for the probability of a type I error
#' @param alternative Character string indicating whether the alternative hypothesis is "two.sided", "less", or "greater"
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family xmar functions
#' @export
bivariate <- function(data,  y, x, title, success = "Non-Traditional",
                      conf = 0.95, alpha = 0.05, alternative = "two.sided") {

  # Create frequency and proportion contingency tables
  freqTbl <- table(data)
  propTbl <- prop.table(table(data), 2)

  # Conduct chi-square test for association
  x2 <- x2Test(freqTbl, y = y, x = x, conf = conf, alpha = alpha)

  # Conduct difference in proportion tests
  dp <- diffTest(data, alternative = alternative,
                 success, conf = conf, alpha = alpha)

  # Plot Data
  plotData <- formatData(x2$htest)
  observed <- plotBars(plotData, y = y, x = x, title = paste("Observed", title))
  x2Plot <- plotX2(x2$result)

  # Format results
  analysis = list(
    tables = list(
      obsFreq = x2$htest$observed,
      expFreq = x2$htest$expected,
      obsProp = round(prop.table(x2$htest$observed), 3),
      expProp = round(prop.table(x2$htest$expected), 3)
    ),
    tests = list(
      x2 = x2,
      dp = dp
    ),
    plots = list(
      observed = observed,
      x2Plot = x2Plot
    )
  )
  return(analysis)
}
