#==============================================================================#
#                                  analyzeTrend                                #
#==============================================================================#
#' analyzeTrend
#'
#' \code{analyzeTrend} Provides numeric summary of series data.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#'
#' @param tbl Table containing series data
#' @param x Cheracter string indicating the explanatory variable
#' @param y Cheracter string indicating the response variable
#' @param z Cheracter string indicating the level of the control variable
#' @param dim Character string indicating the dimension of the table from which the series data is extracted
#'
#' @return data frame of summary statistics
#'
#' @family xmar functions
#' @export
analyzeTrend <- function(tbl, x, y, z, dim) {

  # Prepare data
  propTbl <- prop.table(tbl, 2)

  df <- as.data.frame(propTbl)
  f <- sapply(df, is.factor)
  df[f] <- lapply(df[f], as.character)
  df$Year <- as.numeric(df$Year)
  df <- df %>% filter(Opinion == dim)

  # Get Min and Max Year
  maxYear <- as.numeric(head((df %>% arrange(desc(Freq)) %>% select(Year)), 1))
  minYear <- as.numeric(head((df %>% arrange(Freq) %>% select(Year)), 1))

  analysis <- df %>%
    summarize(Start = min(Year),
              End = max(Year),
              xMin = min(tbl[2,]),
              xMean = mean(tbl[2,]),
              xMedian = median(tbl[2,]),
              xMax = max(tbl[2,]),
              pMin = round(min(Freq), 2),
              pMean = round(mean(Freq), 2),
              pMedian = round(median(Freq), 2),
              pMax = round(max(Freq), 2),
              First = Freq[1],
              Last = Freq[length(Freq)]) %>%
    mutate(Range = paste0(Start, "-", End),
           Gender = z,
           PctChange = (Last - First) / First * 100,
           YearLow = minYear,
           YearHigh = maxYear)
  analysis <- analysis %>% select(Range, Gender, YearLow, YearHigh,
                                  xMin, xMean, xMedian, xMax, pMin,
                                  pMean, pMedian, pMax, PctChange)

  return(analysis)
}
