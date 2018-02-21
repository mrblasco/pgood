source("scripts/functions.R")
load("data-clean/mgh.RData")

pdf("figs/ratings.pdf")

op <- par(family='serif')
tab <- 100*prop.table(table(voting$vote, voting$treatment.proponent), 2)
barplot(tab, angle=15, density=seq(10, 60, length=5)
				, xlim=c(0, ncol(tab) + 3)
				, ylab="Proportion rating (%)"
				, ylim=c(0, 110)
				, legend=rownames(tab)
				, args.legend =list(
												x=ncol(tab) + 3,
      									y=max(colSums(tab)),
      									bty="n")
				, col=1:5
				, space=.5)

dev.off()
