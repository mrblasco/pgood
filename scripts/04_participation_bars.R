# Barplot showing employee participation 
# by solicitation treatment

#source("scripts/functions.R")
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
barplot2 <- function(p, se, labels, add.grid=TRUE, ...) {
	b <- barplot(p, las=1, ...)
	if (add.grid) {
  	grid(lty=3, nx=NA, ny=NULL)
  	b <- barplot(p, las=1, add=TRUE,...)
	}
	arrows(x0=b, y0=p, y1=p+sign(p)*se, angle=90, length=0.1)
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
sink() # end

# barplot 
pdf("figs/part.bar.pdf")
p <- compute_stats(hc$treatment, hc$submit)
barplot2(p$p, p$se, ylim=c(0, 10), col=gray(1:4/4)
  , ylab="Employee submissions (%)", xlab="Solicitation treatment")
dev.off()

# order success and failures
tab.submit.rev <- t(tab.submit[2:1, ])[c(2,3,1,4), ]
tab.submit.rev

(submit.diff <- 100*compute.diff(tab.submit.rev, 4:1, prop.test, conf.level=.90))
(submit.diff.50 <- 100*compute.diff(tab.submit.rev, 4:1, prop.test, conf.level=.50))

pdf("figs/part.cplot.pdf")

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