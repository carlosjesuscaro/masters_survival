library(survival)
library(tidyverse)
library(maxLik)

dat <-  data.frame(ratID = paste0("rat", 1:5),
                   time = c(55, 50, 70, 120, 110),
                   failure = c(0, 1, 1, 0, 1),
                   group = c(0, 1, 0, 1, 1))

sum(dat$failure)

dat <- rename(dat, x = group)
# x = 0 means rat not being sleep deprived
# x = 1 means rat being sleep deprived

dat.events <- subset(dat, failure == 1)

# Lets define the partial (log-)likelihood function
pLogLik <- function(beta) {
  numerator <- with(dat.events, x * beta)
  denominator <- rep(NA_real_, length(numerator))

  for(j in seq_along(denominator)) {
    tj <- dat.events[j, "time"]
    risk_set <- subset(dat, time >= tj)
    theta_j <- with(risk_set, exp(x * beta))
    denominator[j] <- log(sum(theta_j))
  }

  return(sum(numerator - denominator))
}

debugonce(pLogLik)
pLogLik(-1)

# Plotting
f <- Vectorize(pLogLik)
curve(f, from = -4, to = 4)

## Maximum partial-Likelihood estimation
fit.ML <- maxLik(pLogLik, start = c(beta = 0))
summary(fit.ML)
# The estimate value (beta) is -0.5493
# The ratio between rats been sleep deprived or not is exp(beta)
exp(fit.ML$estimate)

# With the `coxph` function:
fit.cph <- coxph(Surv(time, failure) ~ x, data = dat)
summary(fit.cph)
# the p-value comes from the Wald test. H0 = beta=0

# Observations
# The hazard (risk) in sleep deprived is lower (quantified by beta)
# The ratio between being sleep deprived or not is:
# exp(beta) ==> 0.58

# Another example
dat <- data.frame(time = c(6, 7, 10, 15, 19, 25),
                  event = c(1, 0, 1, 1, 0, 1),
                  age = c(67, 62, 34, 41, 46, 28))

fit <- coxph(Surv(time, event) ~ age, data = dat)
summary(fit)
# Interpretation
# 1. p-value is 30% so the efefct of age on risk is not significant
# 2. estimated_beta > 0: to higher age, higher risk (in other words. lower
# time to event)

# Multiple covariates
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
         edu_math, children)

# Data is alreadt clean
d

# Building the covariate model
summary(coxph(Surv(tti_m, status) ~ children + age, data = d))
# Ploting
ggplot(d, aes(x = children, y = age)) + geom_boxplot()

# Covariates with more than 2 levels
d |> count(education) |> arrange(desc(n))
coxph(Surv(tti_m, status) ~ education, data = d) |> summary()
# The output is comparing the ratio with level that was ommited as it used
# to be the reference

# If we want to choose the reference level manually
d3_relevelled <- d |> mutate(education = factor(education)) |>
  mutate(education = relevel(education, ref = "math"))

# Case study: pharmacoSmoking dataset

library(tidyverse)
library(asaur)
library(survival)
dat <- pharmacoSmoking
head(dat)

table(dat$grp)

table(dat$gender)
dat$white <- (dat$race == "white") + 0
dat$ft <- (dat$employment == "ft") + 0

set.seed(4321)
i <- seq(nrow(dat))
i.training <- sample(i, size = floor(nrow(dat) / 2), replace = FALSE)
i.testing <- setdiff(i, i.training)
d.training <- dat[i.training, ]
d.testing <- dat[i.testing, ]

# Effect of the treatment
fit.KM <- sc
print(fit.KM)
plot(fit.KM, col = 1:2)
# Validating with logrank test
survdiff(Surv(ttr, relapse) ~ grp, data = dat)
# The difference is statistically significant

# Model training
fit.KM <- coxph(Surv(ttr, relapse) ~ grp, data = dat)
summary(fit.KM)
# Observations
# 1. p-value is very small so it is significant different
# 2. patchonly treatement is 83.1% more chances to relapse

# Flipping the reference
dat2 <- mutate(dat, grp2 = relevel(factor(grp), "patchOnly"))
fit.KM2 <- coxph(Surv(ttr, relapse) ~ grp2, data = dat2)
summary(fit.KM2)
# The chances of relapsing with the combination method is half than
# with patch only