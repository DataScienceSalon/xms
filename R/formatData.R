#==============================================================================#
#                                  formatData                                  #
#==============================================================================#
#' formatData
#'
#' \code{formatData} Render frequency and proportion data
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#'
#' @param x2 Results from chi-square test of association
#'
#' @return List containing observed and expected frequenty data for plotting.
#'
#' @family xmar functions
#' @export
formatData <- function(x2) {

  # Format observed and expected frequency and data
  freqDf <- melt(round(x2$observed, 0))

  df <- as.data.frame(freqDf %>% group_by(.[[2]]) %>%
                        mutate(Ttl = sum(value)))
  df <- as.data.frame(df %>% arrange(.[[2]], desc(.[[1]])) %>%
                        mutate(posFreq = value - (0.5 * value),
                               Pct = round(value / Ttl * 100, 0),
                               posProp = Pct - (0.5 * Pct),
                               Prop = value / Ttl) %>%
                        select((.[[1]]), Opinion, value,
                               posFreq, posProp, Ttl, Prop, Pct))



  return(df)
}
