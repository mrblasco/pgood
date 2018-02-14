source("scripts/functions.R")
load("data-clean/mgh.RData")

pdf("figs/dynamics.pdf")

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

dev.off()