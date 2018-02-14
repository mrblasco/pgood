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