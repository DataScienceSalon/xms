#==============================================================================#
#                                 preprocess                                   #
#==============================================================================#
#' preprocess
#'
#' \code{preprocess} Creates additional variables required for analysis.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family xmar functions
#' @export
preprocess <- function(GSS) {

  #---------------------------------------------------------------------------#
  #                     Create New Data Set with Factors                      #
  #---------------------------------------------------------------------------#
  xms <- GSS %>%
    dplyr::mutate(
      Year = YEAR,
      Opinion = ifelse(XMARSEX %in% c(1,2), "Traditional",
                       ifelse(XMARSEX  %in%  c(3,4),
                              "Non-Traditional", "NA")),
      Class = ifelse(CLASS_ == 1, "Lower Class",
                     ifelse(CLASS_ == 2, "Working Class",
                            ifelse(CLASS_ == 3, "Middle Class",
                                   ifelse(CLASS_ == 4, "Upper Class",
                                          "NA")))),
      Attend = ifelse(ATTEND == 0, "Never",
                      ifelse(ATTEND == 1, "Lt Once a Year ",
                             ifelse(ATTEND == 2, "Once a Year",
                                    ifelse(ATTEND == 3, "Several Times a Year",
                                           ifelse(ATTEND == 4, "Once a Month",
                                                  ifelse(ATTEND == 5, "2-3 Times a Month",
                                                         ifelse(ATTEND %in% c(6,7), "Every Week",
                                                                ifelse(ATTEND == 8, "More than Once a Week",
                                                                       "NA")))))))),
      Religion = ifelse(RELIG == 1, "Protestant",
                        ifelse(RELIG == 2, "Catholic",
                               ifelse(RELIG == 3, "Jewish",
                                      ifelse(RELIG == 4, "None",
                                             ifelse(RELIG == 11, "Christian",
                                                    ifelse(RELIG %in% c(98,99), "NA",
                                                           "Other")))))),
      `Political Party` = ifelse(PARTYID %in% c(0,1), "Democrat",
                     ifelse(PARTYID %in% c(2,3,4), "Independent",
                            ifelse(PARTYID %in% c(5,6), "Republican",
                                   ifelse(PARTYID == 7, "Other",
                                          "NA")))),

      Region = ifelse(REGION %in% c(1,2), "Northeast",
                      ifelse(REGION %in% c(3,4), "Midwest",
                             ifelse(REGION %in% c(5,6,7), "South",
                                    ifelse(REGION == 8, "Mountain",
                                           "Pacific")))),

      Gender = ifelse(SEX == 1, "Male",
                      ifelse(SEX == 2, "Female", "NA")),
      Age = ifelse(AGE < 25, "15-24",
                   ifelse(AGE < 45, "25-44",
                          ifelse(AGE < 65, "45-64",
                                 ifelse(AGE < 98, "65+", "NA")))),

      `Marital Status` = ifelse(MARITAL == 1, "Married",
                       ifelse(MARITAL == 2, "Widowed",
                              ifelse(MARITAL == 3, "Divorced",
                                     ifelse(MARITAL == 4, "Separated",
                                            ifelse(MARITAL == 5, "Never Married",
                                                   "NA"))))),
      TV = TVHOURS)



  #---------------------------------------------------------------------------#
  #                     Define and Order Variable Factors                     #
  #---------------------------------------------------------------------------#
  xms$Opinion <- factor(xms$Opinion, levels = c("Traditional", "Non-Traditional"))
  xms$Class <- factor(xms$Class, levels = c("Lower Class", "Working Class",
                                             "Middle Class", "Upper Class"))
  xms$Attend <- factor(xms$Attend, levels = c("Never", "Lt Once a Year",
                                              "Once a Year", "Several Times a Year",
                                              "Once a Month", "2-3 Times a Month",
                                              "Every Week", "More than Once a Week"))
  xms$Religion <- factor(xms$Religion, levels = c("Catholic", "Christian",
                                                  "Jewish", "Protestant",
                                                  "Other"))
  xms$`Political Party` <- factor(xms$`Political Party`, levels = c("Democrat", "Independent",
                                            "Republican", "Other"))
  xms$Region <- factor(xms$Region,
                       levels = c("Northeast", "South",
                                  "Midwest", "Mountain", "Pacific"))
  xms$Gender <- factor(xms$Gender, levels = c("Male", "Female"))

  xms$Age <- factor(xms$Age, levels = c("15-24", "25-44", "45-64", "65+"))
  xms$`Marital Status` <- factor(xms$`Marital Status`, levels = c("Married", "Widowed",
                                                "Divorced", "Separated",
                                                "Never Married"))


  #---------------------------------------------------------------------------#
  #                                Univariate                                 #
  #---------------------------------------------------------------------------#
  opinionVar <- xms %>% select(Opinion) %>% filter(Opinion != "NA")
  classVar <- xms %>% select(Class) %>% filter(Class != "NA")
  attendVar <- xms %>% select(Attend) %>% filter(Attend != "NA")
  religionVar <- xms %>% select(Religion) %>% filter(Religion != "NA")
  partyVar <- xms %>% select(`Political Party`) %>% filter(`Political Party` != "NA")
  regionVar <- xms %>% select(Region) %>% filter(Region != "NA")
  genderVar <- xms %>% select(Gender) %>% filter(Gender != "NA")
  ageVar <- xms %>% select(Age) %>% filter(Age != "NA")
  maritalVar <- xms %>% select(`Marital Status`) %>% filter(`Marital Status` != "NA")
  tvVar <- xms %>% select(TV) %>% filter(TV < 25 & TV > -1)

  #---------------------------------------------------------------------------#
  #                                Bivariate                                  #
  #---------------------------------------------------------------------------#
  region <- xms %>% filter(Opinion != "NA" & Region != "NA" & Year > 2011) %>%
    select(Opinion, Region)
  clas <- xms %>% filter(Opinion != "NA" & Class != "NA" & Year > 2011) %>%
    select(Opinion, Class)
  region <- xms %>% filter(Opinion != "NA" & Region != "NA" & Year > 2011) %>%
    select(Opinion, Region)
  gender <- xms %>% filter(Opinion != "NA" & Gender != "NA" & Year > 2011) %>%
    select(Opinion, Gender)
  age <- xms %>% filter(Opinion != "NA" & Age != "NA" & Year > 2011) %>%
    select(Opinion, Age)
  marital <- xms %>% filter(Opinion != "NA" & `Marital Status` != "NA" & Year > 2011) %>%
    select(Opinion, `Marital Status`)
  religion <- xms %>% filter(Opinion != "NA" & Religion != "NA" & Year > 2011) %>%
    select(Opinion, Religion)
  attend <- xms %>% filter(Opinion != "NA" & Attend != "NA" & Year > 2011) %>%
    select(Opinion, Attend)
  tv <- xms %>% filter(Opinion != "NA" & TV < 25 & TV > -1 & Year > 2011) %>%
    select(Opinion, TV)
  party <- xms %>% filter(Opinion != "NA" & `Political Party` != "NA" & Year > 2011) %>%
    select(Opinion, `Political Party`)

  #---------------------------------------------------------------------------#
  #                             Multivariate                                  #
  #---------------------------------------------------------------------------#
  year <- xms %>% filter(Opinion != "NA") %>% select(Opinion, Year, Gender)

  data <- list(
    data = xms,
    univariate = list(
      opinion = opinionVar,
      class = classVar,
      attend = attendVar,
      religion = religionVar,
      party = partyVar,
      region = regionVar,
      gender = genderVar,
      age = ageVar,
      marital = maritalVar,
      tv = tvVar
    ),
    bivariate = list(
      region = region,
      class = clas,
      gender = gender,
      age = age,
      marital = marital,
      religion = religion,
      attend = attend,
      tv = tv,
      party = party
    ),
    multivariate = list(
      year = year
    )
  )
  return(data)
}
