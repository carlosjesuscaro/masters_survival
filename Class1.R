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
                               death_date = as.Date(ifelse(!death %in% c("0", "1"), as.Date(death, format = "%m/%d/%y", origin = "1970-01-01"), NA), origin = "1970-01-01"),
                               gender = factor(gender, levels = c("female", "male")))
summary(covid_formatted)