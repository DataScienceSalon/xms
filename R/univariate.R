#==============================================================================#
#                                 univariate                                   #
#==============================================================================#
#' univariate
#'
#' \code{univariate} Performs univariate analysis of variables
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family xmar functions
#' @export
univariate <- function(xmar) {

  analyze <- function(data, var) {

    me <- 0.05
    alpha <- 0.05
    z <- (1-alpha/2)

    stats <- data %>%
      mutate(Level = data[,1],
             `Minimum N` = round(p * (1-p) / (me / qnorm(z))^2, 0),
             N = N,
             Proportion = round(p, 2),
             Cumulative = round(cumsum(Proportion), 2),
             Successes = N,
             Failures = round(sum(N) - N),
             `Confidence Interval (95%)` =
               paste0("[",
                      round(Proportion -
                              (qnorm(z) *
                                 sqrt(Proportion * (1 - Proportion) / N)),3),
                      ", ",
                      round(Proportion +
                              (qnorm(z) *
                                 sqrt(Proportion * (1 - Proportion) / N)),3),
                      "]"),
             pos = N / 2)

    barplot <- ggplot2::ggplot(data = stats,
                               ggplot2::aes(x = Level, y = N, fill = Level)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 24) +
      ggplot2::geom_text(
        data = stats,
        ggplot2::aes(x = Level, y = pos,
                     label = paste0(N, " (",round(Proportion * 100, 0),"%)")),
        colour="black", family="Tahoma", size = 8) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle(paste('Frequency and Proportion of Responses by', var))

    stats <- stats %>% select(Level, `Minimum N`, N, Proportion, Cumulative,
                              Successes, Failures, `Confidence Interval (95%)`)

    analysis <- list(
      stats = stats,
      plot = barplot
    )

    return(analysis)
  }

  # Format Data
  opinion <- as.data.frame(xmar$opinion %>% select(Opinion) %>% group_by(Opinion) %>%
                             summarize(N = n()) %>%
                             mutate(p = N/ sum(N)), row.names = NULL)
  clas <- as.data.frame(xmar$class %>% select(Class) %>% group_by(Class) %>%
                             summarize(N = n()) %>%
                             mutate(p = N/ sum(N)), row.names = NULL)
  attend <- as.data.frame(xmar$attend %>% select(Attend) %>% group_by(Attend) %>%
                          summarize(N = n()) %>%
                          mutate(p = N/ sum(N)), row.names = NULL)
  religion <- as.data.frame(xmar$religion %>% select(Religion) %>% group_by(Religion) %>%
                          summarize(N = n()) %>%
                          mutate(p = N/ sum(N)), row.names = NULL)
  party <- as.data.frame(xmar$party %>% select(`Political Party`) %>% group_by(`Political Party`) %>%
                          summarize(N = n()) %>%
                          mutate(p = N/ sum(N)), row.names = NULL)
  age <- as.data.frame(xmar$age %>% select(Age) %>% group_by(Age) %>%
                         summarize(N = n()) %>%
                         mutate(p = N/ sum(N)), row.names = NULL)
  gender <- as.data.frame(xmar$gender %>% select(Gender) %>% group_by(Gender) %>%
                            summarize(N = n()) %>%
                            mutate(p = N/ sum(N)), row.names = NULL)
  region <- as.data.frame(xmar$region %>% select(Region) %>% group_by(Region) %>%
                            summarize(N = n()) %>%
                            mutate(p = N/ sum(N)), row.names = NULL)
  marital <- as.data.frame(xmar$marital %>% select(`Marital Status`) %>% group_by(`Marital Status`) %>%
                          summarize(N = n()) %>%
                          mutate(p = N/ sum(N)), row.names = NULL)

  # Conduct Analysis
  opinion <- analyze(opinion, "Opinion")
  clas <- analyze(clas, "Class")
  attend <- analyze(attend, "Religious Service Attendance")
  religion <- analyze(religion ,"Religion")
  party <- analyze(party, "Political Party")
  age <- analyze(age, "Age")
  gender <- analyze(gender, "Gender")
  region <- analyze(region, "Region")
  marital <- analyze(marital, "Marital Status")

  # Return analysis
  analysis <- list(
    opinion = opinion,
    class = clas,
    attend = attend,
    religion = religion,
    party = party,
    age = age,
    gender = gender,
    region = region,
    marital = marital
  )
  return(analysis)
}
