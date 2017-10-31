#==============================================================================#
#                                 diffTest                                     #
#==============================================================================#
#' diffTest
#'
#' \code{diffTest} Conducts multiple z-tests for a multi-level categorical explanatory
#' variable on a response variable.
#'
#' @param data data frame containing data to be analyzed, with the response variable as the first variable.
#' @param alternative direction of the alternative hypothesis; "less","greater", or "two.sided"
#' @param success Character string indicating which level of the response to consider the "success"
#' @param conf confidence level, value between 0 and 1
#' @param alpha numeric between 0 and 1, the probability of a type I error
#'
#' @return Data frame containing the results of the z-test
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family xmar functions
#' @export
#'
diffTest <- function(data, alternative = "two.sided", success,
                     conf = 0.95, alpha = 0.05) {

  freqDf <- as.data.frame(addmargins(table(data), 1))
  groups <- as.character(unique(freqDf %>% .[[2]]))
  nGroups <- length(groups)

  # Bonferroni Correction for Multiple Groups
  numTests <- nGroups * (nGroups - 1) / 2
  alpha <- alpha / numTests

  # Compute critical value
  area <- ifelse(alternative == "two.sided", alpha/2, alpha)
  zAlpha <- qnorm(area, lower.tail = FALSE)

  # Initialize variables
  Populations = Contrast = Value = `Z-Score` = c()
  `p-value` = `95% CI` = Significant = `Relative Risk` = c()
  statements <- list()
  plots <- list()
  k <- 1

  for (i in 1:(nGroups-1)) {
    for (j in (i+1):nGroups) {

      #-----------------------------------------------------------------------#
      #                            Perform Tests                              #
      #-----------------------------------------------------------------------#
      # Compute prop.test
      successes <- c(as.numeric(freqDf %>% filter(.[[1]] == success & .[[2]] == groups[i]) %>%
                       select(Freq)),
                     as.numeric(freqDf %>% filter(.[[1]] == success & .[[2]] == groups[j]) %>%
                       select(Freq)))
      totals <- c(as.numeric(freqDf %>% filter(.[[1]] == "Sum"  & .[[2]] == groups[i]) %>%
                               select(Freq)),
                  as.numeric(freqDf %>% filter(.[[1]] == "Sum" & .[[2]] == groups[j]) %>%
                               select(Freq)))
      t <- prop.test(successes, totals, correct = FALSE,
                     alternative = alternative, conf.level = (1 - (alpha / numTests))) # Bonferroni Correction

      # Compute z-score
      pPooled <- sum(successes) / sum(totals)
      sePooled <- sqrt((pPooled * (1-pPooled) / totals[1]) + (pPooled * (1-pPooled) / totals[2]))
      zScore <- ((successes[1] / totals[1]) - (successes[2] / totals[2])) / sePooled
      pValue <- 2 * pnorm(-abs(zScore))

      # Render decision
      if ((alternative == "two.sided" & pValue < (alpha / 2))
          | (alternative != "two.sided" & pValue < alpha)) {
        decision <- "Reject"
      } else {
        decision <- "Fail to Reject"
      }

      # Compute Relative Risk Ratio
      r1 <-  successes[1] / totals[1]
      r2 <- successes[2] / totals[2]
      rr <- r1 / r2

      #-----------------------------------------------------------------------#
      #                            Format Results                             #
      #-----------------------------------------------------------------------#
      Populations[k] <- paste0(groups[i], " - ", groups[j])
      Contrast[k] <- paste0("p",i, " - p",j)
      Value[k] <- as.numeric(round(t$estimate[1] - t$estimate[2], 3))
      `Z-Score`[k] <- round(zScore, 3)
      `p-value`[k] <- round(t$p.value, 3)
      `95% CI`[k] <- paste0("[ ", round(t$conf.int[1], 3), ", ", round(t$conf.int[2], 3), " ]")
      Significant[k] <- ifelse(decision == "Reject", "Yes", "No")
      `Relative Risk`[k] <- round(rr, 2)

      #-----------------------------------------------------------------------#
      #                             Plot Results                              #
      #-----------------------------------------------------------------------#
      plots[[k]] <- plotDiffTest(x = groups[1], y = groups[2], zAlpha, zScore)

      #-----------------------------------------------------------------------#
      #                          Render Statement                             #
      #-----------------------------------------------------------------------#
      statements[[k]] <- list()
      alt <- ifelse(alternative == "two.sided", "not equal to",
                    ifelse(alternative == "less", "less than","greater than"))
      type1 <- ifelse(decision == "Reject", "less than", "greater than")

      ciNote <- ifelse(decision  == "Reject",
                       paste0("Further, the confidence interval for the difference in ",
                              "proportions does not include zero, the null ",
                              "hypothesis value, suggesting that a zero difference ",
                              "in ", tolower(success), " opinion between the groups ",
                              "is outside the ", alpha * 100, "% margin of error.  "),
                       paste0("Further, the confidence interval for the difference in ",
                              "proportions includes zero, suggesting that a zero ",
                              "difference in the proportion of ", tolower(success),
                              " opinion is within the ", alpha * 100, "% margin of ",
                              "error.  "))

      statements[[k]]$type <- paste0("This was a ", (conf * 100), "% confidence, two-proportion z-test ",
                          "of the null hypothesis that the true population proportion of ",
                          tolower(success), " opinion for ", groups[i], " and ",
                          groups[j], " populations are equal.  ")

      if (decision == "Reject") {
        statements[[k]]$conclude <- paste0("The results of the p-value and confidence interval ",
                                "approaches agree. The null hypothesis was ",
                                "rejected with a ", conf * 100, "% confidence, in favor ",
                                "of the alternative hypothesis that the true ",
                                "population proportion of ", tolower(success),
                                " opinion within the ", groups[i], " population is ",
                                alt, " the true proportion of ", tolower(success),
                                " opinion in the ", groups[j], " population. ")
      } else {
        statements[[k]]$conclude <- paste0("The results of the p-value and confidence ",
                                           "interval approaches agree. The null ",
                                           "hypothesis that the true ",
                                           "population proportions of ",
                                           tolower(success),
                                           " opinion within the ",
                                           groups[i], " and ",
                                           groups[j], " populations are equal,
                                           was not rejected. ")
      }

      statements[[k]]$detail <- paste0("the observed difference in the proportion of ",
                            tolower(success), " opinion between the ",
                            groups[i], " and ", groups[j], " respondents was ",
                            as.numeric(round(t$estimate[1] - t$estimate[2], 3)),
                            ", raising a z-score of ", round(zScore, 3),
                            ", as indicated by the red dot on the plot. ",
                            "The probability of encountering a difference in ",
                            "proportions this extreme (p-value) was approximately ",
                            round(t$p.value, 3), ", which is ", type1, " the ",
                            "probability of incorrectly rejecting the null ",
                            "hypothesis.  ", ciNote)

      k <- k + 1
    }
  }

  #---------------------------------------------------------------------------#
  #                     Compile Results and Return                            #
  #---------------------------------------------------------------------------#

  df <- as.data.frame(cbind(Populations, Contrast, Value, `Z-Score`,
                            `p-value`, `95% CI`, Significant,
                            `Relative Risk`), stringsAsFactors = FALSE)

  res <- list(
    sig = list(
      conf = conf,
      alpha = alpha,
      zAlpha = zAlpha
    ),
    result = df,
    statements = statements,
    plots = plots
  )

  return(res)
}

