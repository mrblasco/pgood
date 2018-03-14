source("scripts/lib/functions.R")
load("data-clean/mgh.RData")

# PLOT quality differences
pdf("figs/quality.pdf")
par(mfrow=c(2,2), bty="n")

# Average 
vote.mean <- aggregate(vote ~ idea_id + treatment.proponent, data=voting, function(x)mean(as.numeric(x)))
boxplot(vote ~ treatment.proponent, vote.mean, ylab="Average rating", cex.axis=0.85)
title("Aggregated ratings")  

# Proportion of ratings by treatment
plot(NA, NA, ylim=c(0, 0.3), xlim=c(1, 5)
  , xlab="Quality\n(1=Low, 5=High)", ylab="% choices", bty="n")
for(i in 1:4) {
  x <- ratings[, i] / sum(ratings[, i])
  lines(x, lty=i, pch=i, type="b")
}
legend("bottom", colnames(ratings), pch=1:4, lty=1:4, bty="n",  ncol=2)
title("Disaggregated ratings")  

dev.off()

#
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
