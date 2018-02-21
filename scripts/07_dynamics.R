source("scripts/functions.R")
load("data-clean/mgh.RData")

pdf("figs/dynamics.pdf")

tab <- table(hc$treatment, hc$week_submission)
# par(family="serif")
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