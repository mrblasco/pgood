# A simple multiple linear regression model 
# to check whether differences in participation 
# can be explained by differences in the gender, 
# profession, and organizational role of the employe

source("scripts/lib/config.R")
library(stargazer)

### FUNCTIONS ##############


### MAIN PROGRAM #############

# estimate probability of submitting
fit <- rep()
fit$baseline  <- glm(100*proposal_y ~ treatment2, data=hc)
fit$ctrl1     <- update(fit$baseline, ~ .  + job2)
fit$ctrl2     <- update(fit$baseline, ~ .  + gender)
fit$ctrl3     <- update(fit$baseline, ~ .  + has_office)
fit$full      <- update(fit$baseline, ~ . + gender + job2 + has_office)

# print results
lapply(fit, summary)

# save fitted model 
part.mlr.fit <- fit 

# test significance of controls
part.mlr.fit.chisq <- anova(part.mlr.fit$baseline, part.mlr.fit$full, test='Chisq')
part.mlr.fit.chisq

# extract mlr coefficients
# cf.baseline <- round(coef(part.mlr.fit$baseline), 1)
# cf.full <- round(coef(part.mlr.fit$full), 1)

# model with interactions 
fit <- rep()
fit$baseline    <- glm(100*proposal_y ~ treatXgender, data=hc)
fit$ctrl1       <- update(fit$baseline, ~ . + job2)
fit$ctrl2       <- update(fit$baseline, ~ . + job2 + has_office)

# save fitted model
part.mlr.fit.interact <- fit

# loglikelihood
part.mlr.fit.interact.ll <- sapply(part.mlr.fit.interact, logLik)

# Additional controls with arbitrary imputation (not reported)
# hc$age.imp <- factor(hc$age, exclude=FALSE)
# hc$tenure.imp <- factor(hc$tenure%/%10, exclude=FALSE)

# print results as latex table
sink("tables/part.mlr.tex")

mynotes <- "This table reports OLS estimates with heteroskedasticity robust standard errors in parenthesis. All coefficients are multiplied by 100 to indicate a percentage point change in the probability of submitting. Solicitation treatment dummies are coded to indicate deviations from the overall probability of submitting. The asterisks $^{\\ast\\ast\\ast}$, $^{\\ast\\ast}$, $^{\\ast}$ indicate significance at 1, 5 and 10 percent level, respectively."

float(part.mlr.fit, se=sapply(part.mlr.fit, robust_se)
			, caption="Probability of submitting proposals"  
      , label="participation ols"
      , notes=mynotes
      , covariate.labels = c("PRIZE", "WPLACE", "FUND"
                            , "Job (nursing)", "Job (MD)"
                            , "Male (yes)", "Office (yes)")
      , dep.var.labels   = " $SUBMIT_{ij}=1$ "
      , keep.stat=c('n')
      , digits=2
      , add.lines=list(c("Log Likelihood", round(sapply(part.mlr.fit, logLik)))))

sink()


# print results as latex table
sink("tables/part.mlr.interact.tex")

mynotes <- "This table reports OLS estimates with heteroskedasticity robust standard errors in parenthesis. All coefficients are multiplied by 100 to indicate the percentage point change in the probability of submitting. Solicitation treatment dummies are coded to indicate deviations from the overall probability of submitting. The asterisks $^{\\ast\\ast\\ast}$, $^{\\ast\\ast}$, $^{\\ast}$ indicate significance at 1, 5 and 10 percent level, respectively."

float(part.mlr.fit.interact, se=sapply(part.mlr.fit.interact, robust_se)
			, caption="Gender differences"  
      , label="participation ols interactions"
      , notes=mynotes
      , covariate.labels = c("PRIZE$\\times$female"
              , "PCARE$\\times$female"
              , "FUND$\\times$female"
              , "WPLACE$\\times$female"
              , "PRIZE$\\times$male"
              , "PCARE$\\times$male"
              , "FUND$\\times$male")
      , dep.var.labels   = " $SUBMIT_{ij}=1$ "
      , keep.stat=c('n'), digits=2
      , omit='job2|has_office'
      , add.lines=list(c("Job", c("no", "yes", "yes"))
                  , c("Office", c("no", "no", "yes"))
                  , c("Log Likelihood", part.mlr.fit.interact.ll))
      )
sink()
