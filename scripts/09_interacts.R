library(stargazer)
source("scripts/functions.R")
load("data-clean/mgh.RData")

pdf("figs/interactions.pdf", width=9, height=5)

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

dev.off()
