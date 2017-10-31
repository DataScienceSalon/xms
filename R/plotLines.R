#==============================================================================#
#                                    plotLines                                  #
#==============================================================================#
#' plotLines
#'
#' \code{plotLines} Renders a two dimensional line plot of proportions of
#' opinion by gender
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#'
#' @param data Data frame with Opinion, Gender and Year
#' @param y Character string indicating the name of the response variable.
#' @param x Character string indicating the name of the explanatory variable.
#' @param title Character string indicating the title of the plot
#'
#' @family xmar functions
#' @export
#'
plotLines <- function(data, y, x, title) {

  #---------------------------------------------------------------------------#
  #                                Line Plot                                  #
  #---------------------------------------------------------------------------#
  # Format data
  f <- sapply(data, is.factor)
  data[f] <- lapply(data[f], as.character)
  male <- subset(data, Gender == "Male")
  female <- subset(data, Gender == "Female")
  maleTbl <- prop.table(table(male), 2)
  femaleTbl <- prop.table(table(female), 2)
  maleDf <- data.frame(Year = as.numeric(dimnames(maleTbl)$Year),
                       Gender = rep("Male", length(dimnames(maleTbl)$Year)),
                       Proportion = as.numeric(maleTbl[1,,]))
  femaleDf <- data.frame(Year = as.numeric(dimnames(femaleTbl)$Year),
                       Gender = rep("Female", length(dimnames(femaleTbl)$Year)),
                       Proportion = as.numeric(femaleTbl[1,,]))
  df <- rbind(maleDf, femaleDf)

  # Render plot
  p <- (ggplot2::ggplot(data = df,
                               ggplot2::aes(x = Year, y = Proportion,
                                            colour = Gender))
  + ggplot2::geom_line()
  + ggplot2::theme_minimal(base_size = 24)
  + ggplot2::theme(legend.position="bottom",
                   text=ggplot2::element_text(family="Open Sans"))
  + ggplot2::scale_colour_manual(values = c('blue', 'darkgreen'))
  + ggplot2::labs(title = title, x = x, y = y))

  return(p)
}
