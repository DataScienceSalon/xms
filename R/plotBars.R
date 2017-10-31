#==============================================================================#
#                                    plotBars                                  #
#==============================================================================#
#' plotBars
#'
#' \code{plotBars} Renders segmented bar plots with value labels representing
#' proportions each segment represents.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#'
#' @param data Data frame containing proportions comprised of two columns: 'target' and 'group'.
#' 'target' represents the proportions for the target variable and 'group' is the name of the grouping variable
#' @param y Character string indicating the name of the response variable.
#' @param x Character string indicating the name of the explanatory variable.
#' @param title Character string indicating the title of the plot
#'
#' @family xmar functions
#' @export
#'
plotBars <- function(data, y, x, title) {

  #---------------------------------------------------------------------------#
  #                                Barplot                                    #
  #---------------------------------------------------------------------------#
  # Proportion Bar Plot

  # Frequency Plot
  p1 <- ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = data[[1]], y = value, fill = data[[2]]),
                      data = data, stat = 'identity') +
    ggplot2::geom_text(data = data,
                       ggplot2::aes(x = data[[1]], y = posFreq,
                                    label = paste0(value, " (",Pct,"%)")),
                       colour="black", family="Tahoma", size = 8) +
    ggplot2::theme_minimal(base_size = 24) +
    ggplot2::theme(legend.position="bottom",
                   text=ggplot2::element_text(family="Open Sans")) +
    ggplot2::scale_fill_brewer(palette = 'Greens', name = y) +
    ggplot2::labs(title = title,
                  x = x,
                  y = y)

  # Proportion Stacked Plot
  p2 <- ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = data[[1]], y = Pct, fill = data[[2]]),
                      data = data, stat = 'identity') +
    ggplot2::geom_text(data = data,
                       ggplot2::aes(x = data[[1]], y = posProp,
                                    label = paste0(value, " (",Pct,"%)")),
                       colour="black", family="Tahoma", size = 8) +
    ggplot2::theme_minimal(base_size = 24) +
    ggplot2::theme(legend.position="bottom",
                   text=ggplot2::element_text(family="Open Sans")) +
    ggplot2::scale_fill_brewer(palette = 'Greens', name = y) +
    ggplot2::labs(title = title,
                  x = x,
                  y = y)

  p <- list(bar = p1, norm = p2)
  return(p)
}
