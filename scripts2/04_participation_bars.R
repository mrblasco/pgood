# Barplot showing employee participation 
# by solicitation treatment

source("scripts/functions.R")
load("data-clean/mgh.RData")

#### FUNCTIONS ###############

# For each pair x,y, the function returns a list object
# with proportions, St.Err, obs., and CIs for x and y
compute_stats <- function(x, y) {
	tab <- table(x, y)
	ptab <- prop.table(tab, 1)
	p <- 100*ptab[, 2]
	n <- apply(tab, 1, sum)
	se <- sqrt(p*(100-p)/n)
	lab <- format(p, digits=2)
	return(list(p=p,n=n,se=se,labels=lab))
}

# custom barplot with arrows
barplot2 <- function(p, se, labels, ...) {
	b <- barplot(p, las=1, ...)
	arrows(x0=b, y0=p, y1=p+sign(p)*se, angle=90, length=0.1)
}

#### MAIN PROGRAM ###############

# compute contingency table of submissions
hc$submit <- hc$num_ideas>0
tab.submit <- xtabs(~ifelse(submit,"Yes","No")+treatment, data=hc)
tab.submit.fisher <- fisher.test(tab.submit)
tab.submit.fisher

# additional rows for latex table
add <- list()
add$pos <- list(-1)
add$cmd <- c("& \\multicolumn{4}{c}{\\emph{Solicitation treatment:}}\\\\
						\\cmidrule(lr){2-5} Submission")

# print as latex table
sink("tables/submit.tex")
table_render(tab.submit
						, caption="Employee participation by solicitation treatment"
						, label="submit"
						, add=add)
sink()

# order success and failures
tab.submit.rev <- t(tab.submit[2:1, ])

# estimate pairwise differences
compute.diff <- function(tab, levels, FUN, ...) {
  diff.est <- apply(combn(levels,2), 2, function(x) {
    y <- FUN(tab[x, ])$estimate
    if(length(y)>1) diff(rev(y))
    else y
    })
  diff.ci <- apply(combn(levels,2), 2, function(x) FUN(tab[x, ], ...)$conf.int)
  diff.labels <- apply(combn(levels,2), 2, function(x) paste(rownames(tab)[x], collapse=" - "))
  df.diff <- data.frame(estimate=diff.est, low=diff.ci[1, ], up=diff.ci[2, ])
  rownames(df.diff) <- diff.labels
  return(df.diff)
}
(submit.diff <- compute.diff(tab.submit.rev, 4:1, prop.test, conf.level=.90))
# submit.diff <- compute.diff(tab.submit.rev, 4:1, fisher.test)
# submit.diff <- data.frame(apply(submit.diff, 2, log)) # log 
n <- nrow(submit.diff) 
plot(submit.diff$est, y=1:n
  , xlim=range(submit.diff), ylim=c(0, n)
  , pch=16, yaxt="n", ylab="", bty="n"
  , xlab="Difference participation rates")
text(y=1:n, x=submit.diff$est, labels=rownames(submit.diff), pos=1, xpd=TRUE)
segments(x0=submit.diff$low, x1=submit.diff$up, y0=1:n)
abline(v=0, lty=2)

# compute participation rates by treatment
out <- compute_stats(hc$treatment, hc$num_ideas>0)

# save plot as pdf
pdf("figs/participation.pdf")

barplot2(out$p, out$se, out$labels
				, angle=15, density=seq(10, 50, length=4)
				, ylab="Employee participation (%)"
				, ylim=range(0, out$p+2*out$se)
				, space=.5)

dev.off()