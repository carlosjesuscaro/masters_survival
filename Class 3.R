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