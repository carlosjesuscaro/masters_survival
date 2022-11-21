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

# Preparing the data for logistic regression based on gender (binaryP: male or female)
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
# ** The reason to use the exponential value of beta_one is to show the data at the scale of the observation

# Logistic regression based on age
glm(death_status ~ age, data = covid_formatted, family = 'binomial') %>%
  tidy(conf.int = TRUE) %>%
  mutate(OR = exp(estimate), OR.L95 = exp(conf.low), OR.U95 = exp(conf.high))

# Reporting:
# 1. The risk of dying by COVID based on age is significant
# 2. The estimate of beta_one is 0.0799 which means that the observation to variable ratio is e^0.0799 = 1.08.
# In other words, every year in age represents an increase in 8% chances to die

# Trying with age based on decades
covid_ageDec <- mutate(covid_formatted, age_decades = age / 10)
glm(death_status ~ age_decades, data = covid_ageDec, family = 'binomial') %>%
  tidy(conf.int = TRUE) %>%
  mutate(OR = exp(estimate), OR.L95 = exp(conf.low), OR.U95 = exp(conf.high))
# The observation to variable ratio shows that every additional decade more than doubles the chances of
# dying of covid

# Analyzing with age treated as a categorical variable: older than 70 y.o or not
covid_cat <- mutate(covid_formatted, age_cat = factor(age > 70, levels = c(FALSE, TRUE), labels = c("<=70", ">70")))
with(covid_cat, table(death_status, age_cat))
glm(death_status ~ age_cat, data = covid_cat, family = 'binomial') %>%
  tidy(conf.int = TRUE) %>%
  mutate(OR = exp(estimate), OR.L95 = exp(conf.low), OR.U95 = exp(conf.high))
# Reporting: the odds of people older than 70 years old is 7.5 times higher of dying of covid

# Does th effect of age ADD to the effect of gender?
glm(death_status ~ age_decades + gender, data = covid_ageDec, family = 'binomial') %>%
  tidy(conf.int = TRUE) %>%
  mutate(OR = exp(estimate), OR.L95 = exp(conf.low), OR.U95 = exp(conf.high))
