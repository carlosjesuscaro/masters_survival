# Non parametric methods for survival analysis

library(survival)
library(tidyverse)

dat <-  data.frame(ratID = paste0("rat", 1:5),
                    time = c(55, 50, 70, 120, 110),
                    status = c(0, 1, 1, 0, 1))

fit.KM <- survfit(Surv(time, status)~1, data = dat)
summary(fit.KM)
# Observations
# Status: 0 means censored and 1 means the event happened (not censored)
plot(fit.KM, mark.time = TRUE,
     main = "Kaplan Meier estimator",
     ylab = "Survival probability",
     xlab = "Time (seconds)")

# The median is the time when half the rats had the event
fit.KM

# Plotting the CDF
plot(fit.KM, fun = "F")
# CDF = 1 - Kaplan-Meir estimator

# Case study: DSTI students time to internship

# Libraries
library(tidyverse)
library(lubridate)
library(broom)
library(survival)

# Utility functions and definitions
as_tibble.survfit <- function(x, ...){
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
            edu_math, children) %>%
     filter(tti_m >= 0)

# Data is alreadt clean
d
summary(d)

# Overall TTI (time to internship)
fit <- survfit(Surv(tti_m, status)~1, data = d)
fit
plot(fit, fun = "F", xlab = "months", ylab = "Cumulated probability of finding internship")
plot(fit, xlab = "months", ylab = "Cumulated probability of finding internship")
# Observations
# fun = "F" ==> it is going to plot 1 - Surv

# Grouping: Having children
d %>%
  group_by(children) %>%
  summarize(surv = get_surv_table(tti_m, status)) %>%
  unnest(surv) %>%
  ggplot(aes(x = time, y = incidence, color = children)) +
    geom_step(lwd = 2) +
    xlab("Time (months)") +
    ylab("Cumulatibve probability of finding an internship") +
    theme_bw(base_size = 18) +
    theme(legend.pos = "top")

# Experimenting with the dataset
d_dirty <- read_csv("~/Downloads/DSTI_survey.csv")
d_dirty$Cohort
table(d_dirty$Cohort)
barplot(table(d_dirty$Cohort))

cohort_levels <- paste(c("S", "A"), rep(15:20, each = 2), sep = "")
d0 <- d_dirty %>%
   mutate(cohort = factor(Cohort, levels =  cohort_levels))
table(d0$cohort)
barplot(table(d0$cohort))

# Pandemic and children
summary(coxph(Surv(tti_m, status) ~ pre_pandemic + children, data = d))

