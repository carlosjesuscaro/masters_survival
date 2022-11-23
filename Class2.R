# Non parametric methods for survival analysis

library(survival)
library(tidyverse)

dat <-  data.frame(ratID = paste0("rat", 1:5),
                    time = c(55, 50, 70, 120, 110),
                    status = c(0, 1, 1, 0, 1))

fit.KM <- survfit(Surv(time, status)~1, data = dat)
summary(fit.KM)