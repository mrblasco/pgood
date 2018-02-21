source("scripts/functions.R")
load("data-clean/mgh.RData")
require(stargazer)

sink("figs/ratings_ols.tex") 

fit <- rep()
fit$m1 <- lm(num_voted_ideas>0 ~ treatment, data=hc)
fit$m2 <- update(fit$m1, ~ job)
fit$m3 <- update(fit$m1, ~ gender)
fit$m4 <- update(fit$m1, ~ has_office)
fit$m6 <- update(fit$m1, ~ . + job + gender + has_office)
# fit$m5 <- update(fit$m1, ~ I(num_ideas>0))
# fit$m7 <- update(fit$m1, ~ . + I(num_ideas>0))
# fit$m8 <- update(fit$m1, ~ . + job + gender + has_office + I(num_ideas>0))

float(fit, coef=sapply(fit, function(x) 100*coef(x))
					, se=sapply(fit, function(x) 100*robust_se(x))
					, keep.stat="n", digits=1)

sink()