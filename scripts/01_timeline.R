# plot timeline of the experiment
# source("scripts/functions.R")
load("data-clean/mgh.RData")

# data about events
d.timing <- data.frame(matrix(c(
	"2014-07-28","Innovation Contest Launch & Submission opens",
	"2014-08-22","Submission Closes",
	"2014-08-27","Peer-Evaluation Opens",
	"2014-09-05","Peer-Evaluation Closes",
	"2014-09-12","Winners Announced & Invited Selections Announced",
	"2014-10-03","Final Round Implementation Plans Due",
	"2014-10-30","Final Award Event & Grant Winners Announced"
), 7, byrow=TRUE))
colnames(d.timing) <- c("date", "activity")
d <- as.Date(d.timing$date)
a <- string.break(d.timing$activity)

# save the plot in pdf format
pdf("figs/timeline.pdf", width=10, height=5)

par(mar=c(4.1,2.1,4.1,2.1))
xlim <- range(c(d+15, d-5))
plot(d, rep(1,7), pch=16, bty="n", yaxt="n", xaxt="n", ann=FALSE
	, ylim=c(-2, 2), xlim=xlim)
yrand <- c(-1.4, -0.8, 2, 1.5, -1.8, 1.5, 1.5)
text(x=d, y=yrand, a, pos=4, xpd=TRUE)
segments(x0=d, y0=1, y1=yrand)
segments(x0=d, x1=d+1, y0=yrand, y1=yrand)
abline(h=1)
x.sq <- as.Date(c("2014-08-01", "2014-09-01", "2014-10-01", "2014-11-01"))
text(x=x.sq, y=1, format(x.sq, "%b"),pos=1)
points(x=x.sq, y=rep(1, 4), pch=3)

dev.off()
