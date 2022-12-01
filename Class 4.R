# Predictions
# 1. Building the model
# Libraries
library(tidyverse)
library(lubridate)
library(broom)
library(survival)

# Utility functions and definitions
as_tibble.survfit <- function(x){
  tibble (
    time = c(0, x$time),
    surv = c(1, x$surv),
    incidence = 1 - surv
  )
}

get_surv_table <- function(time, status){
  survfit(Surv(time, status)~1) %>%
    as_tibble() %>%
    list()
}

edu_labels <- tibble(
  `Education: background (pick a main one you identify with)` =
    c("Business, Management", "Finance, Economy", "Literature, History,
  Philosophy", "Mathematics, Physics, Chemestry, Computer Science,
  Statistics", "Medicine, Biology", "Other"),
  education = c("mgmt", "fin", "lit", "math", "bio", "oth")
)

# Data import
d <- read_csv("~/Downloads/DSTI_survey.csv") %>%
  inner_join(edu_labels, by = "Education: background (pick a main one you identify with)") %>%
  mutate(
    Timestamp = as.POSIXct(Timestamp, format = "%d/%m/%Y %H:%M:%OS"),
    age = year(Timestamp) - `Year of birth`,
    sex = factor(Sex, levels = c("Female", "Male")),
    cohort = factor(Cohort, levels = paste0(c("S", "A"), rep(15:20, each = 2))),
    education = factor(education),
    edu_math = (education == "math") + 0,
    children = factor(`Do you have children?`, levels = c("No", "Yes")),
    cohort_year = 2000 + as.numeric(substring(as.character(cohort), first = 2)),
    pre_pandemic = factor(cohort_year < 2020, levels = c(FALSE, TRUE),
                          labels = c("No", "Yes")),
    intEver = `Have you found an internship?` == "Yes",
    intStart = as.Date(`When did you start looking for an internship`,
                       format = "%m/%d/%Y", origin = "1970-01-01"),
    intStop = as.Date(`When did you stopped looking for an internship`,
                      format = "%m/%d/%Y", origin = "1970-01-01")
  ) %>%
  mutate(
    intStop = as.Date(ifelse(is.na(intStop), as.Date(Timestamp), intStop),
                      origin = "1970-01-01"),
    tti_m = as.numeric(difftime(intStop, intStart, units = "days")) / 30.5,
    status = intEver
  ) %>%
  select(tti_m, status, age, sex, cohort, pre_pandemic, education,
         edu_math, children)

# Data is alreadt clean
d

# Building the covariate model
M1 <- coxph(Surv(tti_m, status) ~ sex + age, data = d)
summary(M1)

# Introducing new data
d_new <- tribble(
  ~name, ~age, ~sex,
  "Antonio", 38, "Male",
  "Carine", 40, "Female"
)
d_new

# Prediction
d_new$lp <- predict(M1, newdata = d_new, type = 'lp')
d_new
d_new |> arrange(lp)
# Observations
# There is a score that y itself is meaningful. However, when compared to
# other values is a rank defining who may take the longest or the shortest
# time to the event

# Calculating a full predicition for new data (not relative to others)
d_luigi <- filter(d_new, name == "Carine")
M1.surv <- survfit(M1, newdata = d_luigi)
M1.surv
# Ploting the survival curve
plot(M1.surv)

# Calculatin the absolute prediction for each element of the new data
M1.surv <- survfit(M1, newdata = d_new)
M1.surv
# Ploting the survival curve
plot(M1.surv, col = 1:2)
