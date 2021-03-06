% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multivariate.R
\name{multivariate}
\alias{multivariate}
\title{multivariate}
\usage{
multivariate(data, y, x, title, success = "Non-Traditional", conf = 0.95,
  alpha = 0.05, alternative = "two.sided")
}
\arguments{
\item{data}{Data frame containing response and explanatory variable}

\item{y}{Character string description for the response variable}

\item{x}{Character string description of the explanatory variable}

\item{title}{Character string containing the plot title}

\item{success}{Character string for the factor variable considered the success factor}

\item{conf}{Numeric between 0 and 1 for the designated confidence level}

\item{alpha}{Numeric between 0 and 1 for the probability of a type I error}

\item{alternative}{Character string indicating whether the alternative hypothesis is "two.sided", "less", or "greater"}
}
\description{
\code{multivariate} Performs a multivariate analysis of opinion by year,
controlling for gender.
}
\seealso{
Other xmar functions: \code{\link{analyze2DData}},
  \code{\link{analyzeTrend}}, \code{\link{bivariate}},
  \code{\link{diffTest}}, \code{\link{plotBars}},
  \code{\link{plotDiffTest}}, \code{\link{plotLines}},
  \code{\link{plotX2}}, \code{\link{preprocess}},
  \code{\link{univariate}}, \code{\link{x2Test}},
  \code{\link{x2TrendTest}}
}
\author{
John James, \email{jjames@datasciencesalon.org}
}
