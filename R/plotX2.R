#==============================================================================#
#                                    plotX2                                   #
#==============================================================================#
#' plotX2
#'
#' \code{plotX2} Renders a plot of the chi-square distribution for a chi-square
#' test with the upper tail shaded.
#'
#' @param x2 Dataframe summarizing results of a chi-square test
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family xmar functions
#' @export
#'
plotX2 <- function(x2) {

  # Format Plot Data
  gg   <- data.frame(x=seq(0,qchisq(p = 0.001, df = x2$d.f., lower.tail = FALSE),
                           max(x2$Critical.Value, x2$X.Squared) / 100))
  gg$y <- dchisq(gg$x,x2$`d.f.`)
  ggNotReject <- subset(gg, x <= x2$Critical.Value)
  ggReject <- subset(gg, x > x2$Critical.Value)
  r1Desc <- data.frame(Result = rep("Fail to Reject", nrow(ggNotReject)))
  r2Desc <- data.frame(Result = rep("Reject", nrow(ggReject)))
  r1 <- cbind(r1Desc, ggNotReject)
  r2 <- cbind(r2Desc, ggReject)
  gg <- rbind(r1,r2)
  x2Point <- data.frame(x = x2$X.Squared,
                        y = dchisq(x2$X.Squared, x2$`d.f.`) / 2)

  # Render Plot
  p <- ggplot2::ggplot(gg) +
    ggplot2::theme_minimal(base_size = 18) +
    ggplot2::scale_fill_brewer(palette = 'Greens', name = "Decision") +
    ggplot2::geom_area(ggplot2::aes(x = x, y = y, fill = Result)) +
    ggplot2::geom_point(data = x2Point,
                        ggplot2::aes(x = x,
                                     y = y),
                        color = "red",
                        show.legend = FALSE,
                        size = 3) +
    ggplot2::geom_text(data = x2Point, family = "Open Sans",
                       ggplot2::aes(x = x * 1.075,
                                    y = y,
                                    label = "X2")) +
    ggplot2::theme(text=ggplot2::element_text(family="Open Sans")) +
    ggplot2::labs(title = paste("Chi-square Test of Independence of",
                                x2$Response, "and", x2$Explanatory),
                  x = "X2",
                  y = "Density")

  return(p)
}
