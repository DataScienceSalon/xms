#==============================================================================#
#                                 plotDiffTest                                 #
#==============================================================================#
#' plotDiffTest
#'
#' \code{plotDiffTest} Renders a plot of the normal distribution plot for
#' difference in proportions test with critical regions shaded.
#'
#' @param x = Character string with name of group associated with proportion 1
#' @param y = Character string with name of group associated with proportion 2
#' @param dp Dataframe summarizing results of a difference in proportions test
#' @param lb Lower bound of the shaded area. Use \code{-Inf} for a left tail.
#' @param ub Upper bound of the shaded area. Use \code{Inf} for a right tail.
#' @param mean Mean of the normal distribution
#' @param sd Standard deviation of the normal distribution
#' @param limits Lower and upper bounds on the x-axis of the area displayed.
#' @return ggplot object.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#'
#' @family xmar functions
#' @export
#'
plotDiffTest <- function(x, y, zAlpha, zScore, lb = -Inf, ub = Inf, mean = 0, sd = 1,
                         limits = c(mean - 3 * sd, mean + 3 * sd)) {

  gg   <- data.frame(x=seq(limits[1], limits[2], length.out = 100))
  gg$y <- dnorm(gg$x)
  ggRejectLow <- subset(gg, x < -zAlpha)
  ggRejectUpper <- subset(gg, x > zAlpha)
  ggNotReject <- subset(gg, x > -zAlpha & x < zAlpha)

  ggRejectLow <- data.frame(Result = rep("Reject", nrow(ggRejectLow)),
                            x = ggRejectLow$x,
                            y = ggRejectLow$y)
  ggRejectUpper <- data.frame(Result = rep("Reject", nrow(ggRejectUpper)),
                            x = ggRejectUpper$x,
                            y = ggRejectUpper$y)
  ggNotReject <- data.frame(Result = rep("Fail to Reject", nrow(ggNotReject)),
                              x = ggNotReject$x,
                              y = ggNotReject$y)
  gg <- rbind(ggRejectLow, ggNotReject, ggRejectUpper)
  zPoint <- data.frame(x = zScore, y = dnorm(zScore) / 2)

  # Render Plot
  p <- ggplot2::ggplot(gg) +
    ggplot2::theme_minimal(base_size = 18) +
    ggplot2::scale_fill_brewer(palette = 'Greens', name = "Decision", direction = -1) +
    ggplot2::geom_area(ggplot2::aes(x = x, y = y, fill = Result)) +
    ggplot2::geom_point(data = zPoint,
                        ggplot2::aes(x = x,
                                     y = y),
                        color = "red",
                        show.legend = FALSE,
                        size = 3) +
    ggplot2::geom_text(data = zPoint, family = "Open Sans",
                       ggplot2::aes(x = x * 1.05,
                                    y = y,
                                    label = "Z")) +
    ggplot2::theme(text=ggplot2::element_text(family="Open Sans")) +
    ggplot2::labs(title = paste("Difference in Proportions of Opinion for",
                                x, "and", y),
                  x = "Z",
                  y = "f(Z)")


  return(p)
}
