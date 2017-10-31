#==============================================================================#
#                                 multivariate                                    #
#==============================================================================#
#' multivariate
#'
#' \code{multivariate} Performs a multivariate analysis of opinion by year,
#' controlling for gender.
#'
#' @param data Data frame containing response and explanatory variable
#' @param y Character string description for the response variable
#' @param x Character string description of the explanatory variable
#' @param z Character string description of the control variable
#' @param title Character string containing the plot title
#' @param success Character string for the factor variable considered the success factor
#' @param conf Numeric between 0 and 1 for the designated confidence level
#' @param alpha Numeric between 0 and 1 for the probability of a type I error
#' @param alternative Character string indicating whether the alternative hypothesis is "two.sided", "less", or "greater"

#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family xmar functions
#' @export
multivariate <- function(data, y, x, z = NULL, title, success = "Non-Traditional",
                      conf = 0.95, alpha = 0.05, alternative = "two.sided") {

  # Format data
  freqTbl <- table(data)
  male <- freqTbl[,,1]
  female <- freqTbl[,,2]

  # Conduct Test
  x2tm <- x2TrendTest(male, y = y, x = x, conf = conf, alpha = alpha)
  x2tf <- x2TrendTest(female, y = y, x = x, conf = conf, alpha = alpha)
  x2tm$result$Sample <- "Male"
  x2tf$result$Sample <- "Female"
  x2t <- rbind(x2tm$result, x2tf$result)
  x2t <- x2t %>% select(Sample, everything())

  # Plots
  observed <- plotLines(data, y = y, x = x, title = title)
  x2tmPlot <- plotX2(x2tm$result)
  x2tfPlot <- plotX2(x2tf$result)

  # Format results
  analysis = list(
    tables = list(
      freqTbl = freqTbl
      ),
    tests = list(
      x2t = x2t,
      x2tm = x2tm,
      x2tf = x2tf
      ),
    plots = list(
      observed = observed,
      x2tmPlot = x2tmPlot,
      x2tfPlot = x2tfPlot
      )
    )
  return(analysis)
}
