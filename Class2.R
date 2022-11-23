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