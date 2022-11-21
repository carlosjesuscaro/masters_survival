# Libraries
library(tidyverse)
library(broom)

# Covid 19 data
covid <- read_csv('~/Downloads/covid19.csv', col_types = cols(id='c', case_in='c',
age='d', if_onset_approximated='l', international_traveler='l', domestic_traveler='l',
traveler='l', `visiting wuhan`='l', `from wuhan`='l', .default='c'))
# Documentation: https://readr.tidyverse.org/reference/read_delim.html
problems(covid)

# There is an issue at row 2225, so it is gonna be removed
covid <- covid[-2224,]

# Checking a specific variable
table(covid$death)
table(covid$gender, useNA = 'always')
hist(covid$age)
summary(covid$age)

# Formatting dates
as_date <- function(x) as.Date(x, format="%m/%d/%y")
covid_formatted <- covid %>% mutate(reporting_date = as_date(reporting_date),
                               hosp_visit_date = as_date(hosp_visit_date),
                               exposure_start = as_date(exposure_start),
                               exposure_end = as_date(exposure_end),
                               symptom_onset = as_date(symptom_onset),
                               death_status = death != "0",
                               death_date = as.Date(ifelse(!death %in% c("0", "1"), as.Date(death, format = "%m/%d/%y",
                                                                                            origin = "1970-01-01"),
                                                           NA), origin = "1970-01-01"),
                               gender = factor(gender, levels = c("female", "male")))
summary(covid_formatted)

# Checking if death_status and gender are independent
covid_formatted$gender
covid_formatted$death_status

# Frequency tables
with(covid_formatted, table(death_status, gender))
with(covid_formatted, round(100*prop.table(table(death_status, gender), 2), 2))

# Independence test
with(covid_formatted, chisq.test(death_status, gender))
with(covid_formatted, fisher.test(death_status, gender))
# Observations:
# In both cases, the p-value is larger than 5% so the variables are independent

# Preparing the data for logistic regression
covid_data <- covid_formatted %>% select(death_status, gender) %>% na.omit()
summary(glm(death_status ~ gender, data = covid_data, family = 'binomial'))
# Observations:
# The intercept value does not really have interpretation.
# The 'gendermale' p-value is over 5% so there isnt significance difference between male and female
# The null hypothesis of the p-values is that the estimated value is 0

beta0 <- -3.16
beta1 <- 0.507
exp(beta1)
# The odds of death in males is 1.66 times the odds of death in females

glm(death_status~gender, data = covid_data, family = 'binomial') %>%
  tidy(conf.int = TRUE) %>%
  transmute(
    term,
    OR = exp(estimate),
    OR.L = exp(conf.low),
    OR.U = exp(conf.high),
    p.value)
# glm(death_status~gender, data = covid_data, family = 'binomial') %>% tidy(conf.int = TRUE)

# Reporting:
# 1. There is no siginifricance difference between male and female related to the death_status
# 2. The ratio between male and female is the exponential of beta1: e^beta1 ~ e^0.5084 = 1.66
# 3. The confidence interval is (95% confidence):
# - lower bound: 1.00
# - upper bound: 2.84
# ** The reason to use the exponential value of beta_one is to show the data at the scale of the observation

