# Tables and figures about employee participation 
# by solicitation treatment
source("scripts/lib/config.R")

#### FUNCTIONS ###############
# stats for proportions
compute_stats <- function(x, y) {
	tab <- table(x, y)
	ptab <- prop.table(tab, 1)
	p <- 100*ptab[, 2]
	n <- apply(tab, 1, sum)
	se <- sqrt(p*(100-p)/n)
	lab <- format(p, digits=2)
	return(list(p=p,n=n,se=se,labels=lab))
}
# more barplots
plot_bars <- function(p, se, labels, ...) {
    b <- barplot(p, las = 1, ...)
    arrows(x0 = b, y0 = p, y1 = p + sign(p) * se, angle = 90, 
        length = 0.1)
}  
# default values for custom barplot
barplot3 <- function(p, ylim=c(0,8), a=15, d=c(10, 50), col=1:2
  , ylab="% employees submitting", xlab="", ...) {
#   barplot2(p$p, p$se, ylim=ylim
#     , col=gray(c(.25,.75)), border=gray(c(.25,.75))
#     , ylab="% employees with submissions"
#     , xlab="", ...)
  plot_bars(p$p, p$se, ylim=ylim, angle=a, density=d, ylab=ylab, xlab=xlab, col=col, ...)
}
# estimate pairwise differences
compute.diff <- function(tab, levels, FUN, ...) {
  diff.est <- apply(combn(levels,2), 2, function(x) {
    y <- FUN(tab[x, ])$estimate
    if(length(y)>1) { 
      diff(rev(y))
    } else { y }
    })
  diff.ci <- apply(combn(levels,2), 2, function(x) FUN(tab[x, ], ...)$conf.int)
  diff.labels <- apply(combn(levels,2), 2, function(x) paste(rownames(tab)[x], collapse=" - "))
  df.diff <- data.frame(estimate=diff.est, low=diff.ci[1, ], up=diff.ci[2, ])
  rownames(df.diff) <- diff.labels
  return(df.diff)
}
# convenient table
tabl2 <- function(tab, labels, digits=1) { 
  tab.count <- apply(tab, 2, function(x) paste("(", x, ")", sep=""))
  tab.pc <- apply(percent(prop.table(tab, 2), digits), 2, function(x) paste(x, "%"))
  tab.final <- rbind(tab.pc, tab.count)[c(1,3,2,4), ]
  data.frame(labels, tab.final)
}

########################################
#### MAIN PROGRAM   ####################
########################################

# Prepare variables
hc$office_y <- ifelse(hc$has_office=="yes","Fixed office", "Ward (no office)")
hc$gender2 <- factor(hc$gender)
levels(hc$gender2) <- capitalize(levels(hc$gender2))


# Test association between submission rates
# and solicitation treatments
tab.submit <- xtabs(~ifelse(submit,"Yes","No")+treatment, data=hc)
(tab.submit.fisher <- fisher.test(tab.submit))

########################################
# Percent submissions
sink("tables/submit.treatment.tex")
out <- tabl2(tab.submit, c("% with no submission","","% with submission",""))
add <- list()
add$pos <- list(-1)
add$cmd <- c("& \\multicolumn{4}{c}{\\emph{Solicitation treatment:}}\\\\
						\\cmidrule(lr){2-5} \\emph{Submission:} &")
table_render(out
    , align=c("@{}l","@{}l", rep("c", ncol(out)-1))
    , caption="Employee participation in the contest"
    , label="submit"
    , add=add
    , include.rownames=FALSE
    , sanitize.colnames.function=function(x)x[-1])
sink()

########################################
# Barplot submission rates by treatment
pdf("figs/part.bar.pdf")
barplot3(compute_stats(hc$treatment, hc$submit), ylim=c(0,10))
dev.off()

########################################
# Barplot submission rates by gender, job, office
pdf("figs/sorting.bar.pdf")
par(mfrow=c(2,2), bty="n") 
barplot3(compute_stats(hc$gender2, hc$submit), main="Gender")
barplot3(compute_stats(hc$job, hc$submit), main="Profession")
barplot3(compute_stats(hc$office_y, hc$submit), main="Office")
dev.off()

########################################
# Compute pairwise differences
tab.submit.rev <- t(tab.submit[2:1, ])[c(2,3,1,4), ]
(submit.diff <- 100*compute.diff(tab.submit.rev, 4:1, prop.test, conf.level=.90))
(submit.diff.50 <- 100*compute.diff(tab.submit.rev, 4:1, prop.test, conf.level=.50))

# Plot pairwise differences
pdf("figs/part.parwise.cplot.pdf")
n <- nrow(submit.diff) 
plot(submit.diff$est, y=1:n
  , xlim=range(submit.diff), ylim=c(0, n)
  , pch=16, yaxt="n", ylab="", bty="n"
  , xlab="Pairwise difference participation (%)")
text(y=1:n, x=submit.diff$est, labels=rownames(submit.diff), pos=1, xpd=TRUE)
segments(x0=submit.diff$low, x1=submit.diff$up, y0=1:n)
segments(x0=submit.diff.50$low, x1=submit.diff.50$up, y0=1:n,lwd=3)
abline(v=0, lty=2)
dev.off()


########################################
# Pairwise testing
(pairwise.diff <- pairwise.prop.test(t(tab.submit), p.adjust.method="none", correct=FALSE))

# Print as latex table
sink("tables/pairwise.tex")
table_render(pairwise.diff$p.val
						, digits=3
						, caption="P-values for pairwise comparison of proportions"
						, label="pairwise"
						, notes="This table reports the p-values of pairwise comparisons of proportions
						         among solicitation treatments.")
sink()

########################################
# Participation dynamics
tab <- table(hc$treatment, hc$week_submission)

pdf("figs/dynamics.pdf")
plot(NA, NA, bty="n", xaxt="n", las=1
  , ylim=c(0, 25), xlim=c(1, 4)
  , ylab="Employees with submission"
  , xlab="Submission week")
abline(h=c(0,5,10,15,20, 25), col=gray(.75), lty=3)
for (i in 1:4) {
  lines(cumsum(tab[i, ]), type='b', lty=5-i, pch=20+i, col=i
      , lwd=2)
  text(x=4.1, y=cumsum(tab[i, ])[4]-0.02, rownames(tab)[i]
  		, pos=ifelse(i==2,1,3), xpd=TRUE, col=i)
}
axis(1, at=1:4)
dev.off()


########################################
# Heterogeneous treatment effects
hc_by_gender <- split(hc, hc$gender)
hc_by_job <- split(hc, ifelse(hc$job=="MD/Fellow","MD","non-MD"))

# Plot
par(mfrow=c(1, 2))
pdf("figs/interact.pdf", width=9, height=5)

# by gender
out <- lapply(hc_by_gender, function(x) compute_stats(x$treatment, x$submit))
p_by_gender <- rbind(out[[1]]$p, out[[2]]$p)
se_by_gender <- rbind(out[[1]]$se, out[[2]]$se)
barplot3(list(p=p_by_gender, se=se_by_gender)
  , beside=TRUE, ylim=c(0, 15)
  , legend=names(out), args.legend=list(bty="n")
  , main="Participation by gender")

# By job
out <- lapply(hc_by_job, function(x) compute_stats(x$treatment, x$submit))
p_by_job <- rbind(out[[1]]$p, out[[2]]$p)
se_by_job <- rbind(out[[1]]$se, out[[2]]$se)
barplot3(list(p=p_by_job, se=se_by_job)
  , beside=TRUE, ylim=c(0, 15)
  , legend=names(out), args.legend=list(bty="n")
  , main="Participation by profession")
dev.off()

