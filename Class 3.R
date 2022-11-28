library(survival)
library(tidyverse)
library(maxLik)

dat <-  data.frame(ratID = paste0("rat", 1:5),
                   time = c(55, 50, 70, 120, 110),
                   failure = c(0, 1, 1, 0, 1),
                   group = c(0, 1, 0, 1, 1))

sum(dat$failure)

dat <- rename(dat, x = group)
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


