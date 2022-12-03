# Predictions
# 1. Building the model
# Libraries
library(tidyverse)
library(lubridate)
library(broom)
library(survival)
library(asaur)

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

# Calculating a full prediction for new data (not relative to others)
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

# Case study: smoking prediction based on DSTI survey
d_testing <- read_csv("~/Downloads/DSTI_survey.csv") |>
  mutate(
    Timestamp = as.POSIXct(Timestamp, format = "%d/%m/%Y %H:%M:%OS"),
    age = year(Timestamp) - `Year of birth`,
    sex = factor(Sex, levels = c("Female", "Male")),
    smkEver = `Were you ever a smoker?` != "No",
    smkAge = ifelse(smkEver, `Year when first started smoking` -
      `Year of birth`, age)) |>
  transmute(age, gender = sex, smkEver, smkAge)
d_testing

library(asaur)
d_training <- pharmacoSmoking

# Model training
M1 <- coxph(Surv(ttr, relapse) ~ age + gender, data = d_training)
summary(M1)
M2 <- coxph(Surv(ttr, relapse) ~ age, data = d_training)
summary(M2)
M3 <- coxph(Surv(ttr, relapse) ~ gender, data = d_training)
summary(M3)

# Making predictions for the DSTI dataset
d_testing$pred_m1 <- predict(M1, newdata = d_testing)
d_testing$pred_m2 <- predict(M2, newdata = d_testing)
d_testing$pred_m3 <- predict(M3, newdata = d_testing)

d_testing

# Validating the quality of the predictions
summary(coxph(Surv(smkAge, smkEver) ~ pred_m1, data = d_testing))
# p-value is low so it is not significant: there is no association between
#   prediction and risk
# the coefficient is negative. The expectation should be the higher the score
#   then the higher the risk. However in this case means, the higher the score
#   then the lower the risk so it is in the wrong direction
# Concordance (c statistic - part of the output) is the fraction of pairs that
#   are correctly sorted so it is analogoius to the AUC so it is another
#   measure of performance between models, the higher the better

summary(coxph(Surv(smkAge, smkEver) ~ pred_m2, data = d_testing))
summary(coxph(Surv(smkAge, smkEver) ~ pred_m3, data = d_testing))
# None of the models provides a valid prediction

# As an alternative, we can discretize the predictions
hist(d_testing$pred_m1)
d_testing <- d_testing |>
  mutate(m1_risk = factor(pred_m1 > median(pred_m1, na.rm = TRUE),
                          levels = c(FALSE, TRUE),
                          labels = c("low", "high")))

table(d_testing$m1_risk)
filter(d_testing, is.na(pred_m1))

survfit(Surv(smkAge, smkEver) ~ m1_risk, data = d_testing) |>
  plot(col = 1:2, fun = "F", xlab = "Age (years)", ylab = "Cumulated
  probability of smoking")
  legend("bottomright", col = 1:2, legend = c("low risk", "high risk"), lty = 1)

survdiff(Surv(smkAge, smkEver) ~ m1_risk, data = d_testing)
# LogRank test: p-value  = 0.5 so there is no significant difference between
# low and high risk predictions
summary(coxph(Surv(smkAge, smkEver) ~ m1_risk, data = d_testing))
# the risk of high risk is 33% higher than low risk group

# Another option: binarize the duration
d_bin <- d_testing |> mutate(
  y = ifelse(
    smkAge > 20, 0,
    ifelse (
      smkAge <= 20 & smkEver, 1, NA)
  )
)
d_bin
# Now this is a classification problem. However, the challenge is that
# now we are limiting ourselves to a specific duration

# Choosing relevant categorical variables

# Loading the data
data_smk <- pharmacoSmoking

# Building the models
m0 <- coxph(Surv(ttr, relapse) ~ 1, data = data_smk)
m1 <- coxph(Surv(ttr, relapse) ~ ageGroup4, data = data_smk)
m2 <- coxph(Surv(ttr, relapse) ~ employment, data = data_smk)
m3 <- coxph(Surv(ttr, relapse) ~ ageGroup4 + employment, data = data_smk)

anova(m2, m3)
# small p-value, ageGroup4 is a significant variable
anova(m1, m3)
# large p-value, employment is not a significan variable and can be dropped

