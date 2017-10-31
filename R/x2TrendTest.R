#==============================================================================#
#                                  x2TrendTest                                 #
#==============================================================================#
#' x2TrendTest
#'
#' \code{x2TrendTest} Conducts a chi-square test proportion trend test.
#'
#' @param freqTbl Data to be analyzed with response variable as first variable.
#' @param x Character string indicating the name of the explanatory variable
#' @param y Character string indicating the name of the response variable
#' @param z Chracter string for the level of the control variable
#' @param conf Level of confidence between 0 and 1
#' @param alpha The probability of a type 1 error between 0 and 1
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family xmar functions
#' @export
#'
x2TrendTest <- function(freqTbl, x, y, z, conf = 0.95, alpha = 0.05) {

  propTbl <- prop.table(freqTbl, 2)
  freqTbl <- addmargins(freqTbl, 1)
  x2 <- prop.trend.test(x = as.numeric(freqTbl[2,]),
                        n = as.numeric(freqTbl[3,]))


  # Compute Critical Value
  criticalVal <- qchisq(alpha, x2$parameter, lower.tail = F)

  # Create Table
  table <- data.frame(Period = paste0(as.numeric(dimnames(freqTbl)$Year[1]), "-",
                                      as.numeric(dimnames(freqTbl)$Year[length(freqTbl[2,])])),
                      Response = y,
                      Explanatory = x,
                      Gender = z,
                      `d.f.` = x2$parameter,
                      N = sum(freqTbl[3,]),
                      `Critical Value` = criticalVal,
                      `X-Squared` = x2$statistic,
                      `p-value` = ifelse(x2$p.value < 0.05, "p < 0.05", round(x2$p.value, 3)),
                      Decision = ifelse(x2$p.value >= alpha,"Fail to Reject", "Reject"),
                      row.names = NULL)

  # Format Statements
  stmt <- list()
  stmt$type <- paste0("A chi-square test for trend in proportions was ",
                      "conductedin order to ",
                      "challenge the null hypothesis that no linear trend exists ",
                      "in opinion over time. ")

  if (x2$p.value < (alpha)) {
    stmt$conclude <- paste0("Therefore, the null hypothesis was rejected in ",
                            "favor of the alternative hypothesis which states ",
                            "with ", conf * 100, "% confidence, that opinion ",
                            "changes linearly over time.  ")
  } else {
    stmt$conclude <- paste0("Therefore, the null hypothesis that no linear ",
                            "relationship between opinion and time exists, ",
                            "was not rejected. ")
  }

  stmt$detail <- paste0("the critical value indicated by the shaded region ",
                        "was ", round(criticalVal, 2), ". The sum of ",
                        "the squared differences (indicated by the red dot) ",
                        "between observed counts and the expected counts was ",
                        round(x2$statistic,0), ". With ",
                        x2$parameter, " degrees of freedom and N = ", table$N,
                        ", the probability of encountering a difference ",
                        "this extreme (p-value) was approximately ",
                        round(x2$p.value, 3), ". ")


  test <- list(
    sig = list(
      conf = conf,
      alpha = alpha
    ),
    result = table,
    stmt = stmt
  )
  return(test)
}
