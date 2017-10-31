#==============================================================================#
#                                  analyze2DData                                  #
#==============================================================================#
#' analyze2DData
#'
#' \code{analyze2DData} Render frequency and proportion data
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#'
#' @param x2 Results from chi-square test of association
#'
#' @return List containing observed and expected frequenty data for plotting.
#'
#' @family xmar functions
#' @export
analyze2DData <- function(x2) {

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
  df <- as.data.frame(df %>% group_by(Opinion) %>%
                        mutate(TtlProp = sum(Prop)))
  df <- as.data.frame(df %>% arrange(Opinion, desc(Prop)) %>%
                        mutate(PctProp = Prop / TtlProp * 100))
  df <- as.data.frame(df %>% group_by(Opinion) %>%
                        arrange(Opinion, desc(PctProp)) %>%
                        mutate(CumPct = cumsum(PctProp),
                               RelativePct = PctProp / mean(PctProp) * 100))



  return(df)
}
