######################################## 
# This scripts compute tables and figures 
# for the Results section of the paper
# "Contributing to Public Goods"
######################################## 

#### MAIN PROGRAM ####

######################################## 
## @knitr results_common
######################################## 

# Some basic participation statistics
particip <- sum(hc$num_ideas>0)
particip_percent <- round(100*mean(hc$num_ideas>0))
projects <- sum(hc$num_ideas)
p <- round(100*tapply(hc$num_ideas>0, hc$treatment, mean), 1)
rr_prize_pcare <- round((p["PRIZE"]/p["PCARE"]), 2)
rr_prize_wplace <- round((p["PRIZE"]/p["WPLACE"]), 2)
rr_prize_fund <- round((p["PRIZE"]/p["FUND"]), 2)


######################################## 
## @knitr figure_participation
######################################## 

# Barplot showing employee participation 
# by solicitation treatment
out <- compute_stats(hc$treatment, hc$num_ideas>0)
par(family="serif")
plot_bars(out$p, out$se, out$labels
				, angle=15, density=seq(10, 50, length=4)
				, ylab="Employee participation (%)"
				, ylim=range(0, out$p+2*out$se)
				, space=.5)



######################################## 
## @knitr table_submissions
######################################## 

# Table with full statistics about submissions
# (in the appendix)
f <- function() {
  text <- c("PRIZE", "FUND", "PCARE", "WPLACE")
  m <- table(hc$treatment, hc$num_ideas>0)
  index <- match(text, rownames(m))
  m <- m[index, ]
  m <- rbind(m, "[1.8ex] Total"=apply(m, 2, sum))
  n <-  apply(m , 1, sum)
  p <- round(100*m[, 2] / n, 1)
  m <- cbind(m, p)
  nsub      <- tapply(hc$num_ideas, hc$treatment, sum)
  nsub.med  <- tapply(hc$num_ideas, hc$treatment, function(x) median(x[x>0]))
  nsub.mean  <- tapply(hc$num_ideas, hc$treatment, function(x) mean(x[x>0]))
  index <- match(text, names(nsub))
  nsub <- c(nsub[index], sum(nsub))
  nsub.med <- c(nsub.med[index], median(hc$num_ideas[hc$num_ideas>0]))
  nsub.mean <- c(nsub.mean[index], mean(hc$num_ideas[hc$num_ideas>0]))
  m <- cbind(m, nsub, nsub.mean, nsub.med)
  colnames(m) <- c("No", "Yes", "% yes", "Total", "Mean", "Median")
  return(m)
}
add <- rep()
add$cmd <- "& \\multicolumn{3}{c}{\\emph{Submitting proposals:}}& \\multicolumn{3}{c}{\\emph{Submitted proposals:}} \\\\\n \\cmidrule(lr){2-4}\\cmidrule(lr){5-7}"
add$pos <- -1
table_render(f()
  , caption="Outcomes of the submission phase"
  , label="tab: submissions"
  , digits=c(0, 0, 0, 1, 0, 1, 0), add=add)


######################################## 
## @knitr pairwise_comparisons
######################################## 

# Shows confidence intervals and p-values for all pairs
allpairs <- combn(4, 2)
m <- table(hc$treatment, !hc$num_ideas>0)
m <- m[c(4,1:3), ] # cosmetic reordering
m.lab <- paste(rownames(m)[allpairs[1, ]]
							, rownames(m)[allpairs[2, ]], sep=' - ')
m.test <- apply(allpairs, 2, function(x) {
	prop.test(m[x, ], correct=FALSE)
})
names(m.test) <- m.lab
mat <- rbind(100*sapply(m.test,function(x) x$conf.int)
			, sapply(m.test,function(x) x$p.value))
mat <- t(mat)
colnames(mat) <- c("Lower", "Upper", "P-value")

# Rendering table in latex
table_render(mat
						, digits=3
						, caption="Pairwise difference in participation rates"
						, label="pairwise"
						, notes="This table reports Wald confidence intervals (95 percent level) for the difference in the proportions of submissions between every two solicitation treatments. The last column reports p-values from a two-sided proportion test where the null tested is that, for a given pair, the proportions of submissions in each group are equal.")

######################################## 
## @knitr figure_dynamics
######################################## 

# This plot shows how employee participation evolves over time.
tab <- table(hc$treatment, hc$week_submission)
par(family="serif")
plot(NA, bty="n", xaxt="n", las=1
  , ylim=c(0, 25), xlim=c(1, 5)
  , ylab="Project submissions"
  , xlab="Submission week")
for (i in 1:4) {
  lines(cumsum(tab[i, ]), type='b', lty=5-i, pch=20+i, col=i, lwd=1.5)
  text(x=4.5, y=cumsum(tab[i, ])[4]-0.02, rownames(tab)[i]
  		, pos=3, xpd=TRUE)
}	
axis(1, at=1:4)


######################################## 
## @knitr table_ols
######################################## 

# A simple regression model to check whether differences 
# in participation can be explained by differences 
# in the gender, profession, and organizational role 
# of the employe


# OLS for probability of submitting with robust SE 
fit <- rep()
fit$baseline  <- glm(100*proposal_y ~ treatment2, data=hc)
fit$ctrl1     <- update(fit$baseline, ~ .  + job2)
fit$ctrl2     <- update(fit$baseline, ~ .  + gender)
fit$ctrl3     <- update(fit$baseline, ~ .  + has_office)
fit$full      <- update(fit$baseline, ~ . + gender + job2 + has_office)

# Other controls with arbitrary imputation (not reported)
# hc$age.imp <- factor(hc$age, exclude=FALSE)
# hc$tenure.imp <- factor(hc$tenure%/%10, exclude=FALSE)
# fit$full2      <- update(fit$full, ~ . + age.imp)
# fit$full3      <- update(fit$full, ~ . + tenure.imp)
# fit$full4      <- update(fit$full, ~ . + age.imp + tenure.imp)

# stargazer(fit, type="text", se=sapply(fit, robust_se), digits=1, keep.stat="n")

mynotes <- "This table reports OLS estimates with heteroskedasticity robust standard errors in parenthesis. All coefficients are multiplied by 100 to indicate the percentage point change in the probability of submitting. Solicitation treatment dummies are coded to indicate deviations from the overall probability of submitting. The asterisks $^{\\ast\\ast\\ast}$, $^{\\ast\\ast}$, $^{\\ast}$ indicate significance at 1, 5 and 10 percent level, respectively."

float(fit, se=sapply(fit, robust_se)
			, caption="Probability of submitting proposals"  
      , label="participation ols"
      , notes=mynotes
      , covariate.labels = c("PRIZE", "WPLACE", "FUND"
                            , "Job (nursing)", "Job (MD)"
                            , "Male (yes)", "Office (yes)")
      , dep.var.labels   = " $SUBMIT_{ij}=1$ "
      , keep.stat=c('n'), digits=2
      , add.lines=list(c("Log Likelihood", round(sapply(fit, logLik)))))

# Extract coefficients
cf.baseline <- round(coef(fit$baseline), 1)
cf.full <- round(coef(fit$full), 1)

# Testing the models
p <- anova(fit$baseline, fit$full, test='Chisq')[5]
p <- format(p[2,], digits=3)



######################################## 
## @knitr figure_interactions
######################################## 

# Barplot showing differences in employee participation 
# between men and women, and doctors and non-doctors

# Compute stats conditional on the gender
hc_by_gender <- split(hc, hc$gender)
hc_by_job <- split(hc, ifelse(hc$job=="MD/Fellow","MD","non-MD"))
par(mfrow=c(1, 2), family='serif')

# By gender
out <- lapply(hc_by_gender, function(x) {
							compute_stats(x$treatment, x$num_ideas>0)
							})

p_by_gender <- rbind(out[[1]]$p, out[[2]]$p)
se_by_gender <- rbind(out[[1]]$se, out[[2]]$se)
plot_bars(p_by_gender, se_by_gender, labels=1:4, beside=TRUE
			, angle=15, density=c(10, 50), ylim=c(0, 15)
			, legend=names(out)
			, col=1:2
			, ylab="Employee participation (%)")
title("(a) participation by gender")

# By job
out <- lapply(hc_by_job, function(x) {
							compute_stats(x$treatment, x$num_ideas>0)
							})
p_by_job <- rbind(out[[1]]$p, out[[2]]$p)
se_by_job <- rbind(out[[1]]$se, out[[2]]$se)
plot_bars(p_by_job, se_by_job, labels=1:4, beside=TRUE
			, angle=15, density=c(10, 50), ylim=c(0, 15)
			, legend=names(out)
			, col=1:2
			, ylab="Employee participation (%)")
title("(b) participation by profession")

######################################## 
## @knitr table_interactions
######################################## 

# Other controls
fit <- rep()
fit$baseline    <- glm(100*proposal_y ~ treatXgender, data=hc)
fit$ctrl1       <- update(fit$baseline, ~ . + job2)
fit$ctrl2       <- update(fit$baseline, ~ . + job2 + has_office)
proposal_ols_gender <- fit
SE <- sapply(proposal_ols_gender, robust_se)

# Additional controls with arbitrary imputation (not reported)
# hc$age.imp <- factor(hc$age, exclude=FALSE)
# hc$tenure.imp <- factor(hc$tenure%/%10, exclude=FALSE)

# stargazer(fit, type="text", se=sapply(fit, robust_se), digits=1, keep.stat="n")

mynotes <- "This table reports OLS estimates with heteroskedasticity robust standard errors in parenthesis. All coefficients are multiplied by 100 to indicate the percentage point change in the probability of submitting. Solicitation treatment dummies are coded to indicate deviations from the overall probability of submitting. The asterisks $^{\\ast\\ast\\ast}$, $^{\\ast\\ast}$, $^{\\ast}$ indicate significance at 1, 5 and 10 percent level, respectively."

ll <- round(sapply(proposal_ols_gender, logLik))

float(proposal_ols_gender, se=SE, caption="Gender differences"  
      , label="tab: probability submitting interactions"
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
                  , c("Log Likelihood", ll))
      )
######################################## 
######################################## 
######################################## 
######################################## 
######################################## 
######################################## 