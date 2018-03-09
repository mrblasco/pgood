# Tables and figures about employee participation 
# by solicitation treatment
source("scripts/lib/functions.R")
load("data-clean/mgh.RData")


#### FUNCTIONS ###############
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

percent <- function(x, ...) round(100*x, ...)

tabl2 <- function(tab, labels, digits=1) { 
  tab.count <- apply(tab, 2, function(x) paste("(", x, ")", sep=""))
  tab.pc <- apply(percent(prop.table(tab, 2), digits), 2, function(x) paste(x, "%"))
  tab.final <- rbind(tab.pc, tab.count)[c(1,3,2,4), ]
  data.frame(labels, tab.final)
}

barplot3 <- function(p, ylim=c(0,8)) {
  barplot2(p$p, p$se, ylim=ylim
    , col=gray(c(.25,.75)), border=gray(c(.25,.75))
    , ylab="% employees with submissions"
    , xlab="")
}


#### MAIN PROGRAM ###############

# compute contingency table of submissions
hc$submit <- hc$num_ideas>0
tab.submit <- xtabs(~ifelse(submit,"Yes","No")+treatment, data=hc)
tab.submit.fisher <- fisher.test(tab.submit)
tab.submit.fisher

# Print in latex format
out <- tabl2(tab.submit, c("% with no submission","","% with submission",""))

# additional rows for latex table
add <- list()
add$pos <- list(-1)
add$cmd <- c("& \\multicolumn{4}{c}{\\emph{Solicitation treatment:}}\\\\
						\\cmidrule(lr){2-5} \\emph{Submission:} &")

sink("tables/submit_by_treatment.tex")
table_render(out
  , align=c("@{}l","@{}l", rep("c", ncol(out)-1))
  , caption="Employee participation in the contest"
  , label="submit"
  , add=add
  , include.rownames=FALSE
  , sanitize.colnames.function=function(x)x[-1])
sink()

# barplot of participation rates
pdf("figs/part.bar.pdf")
p <- compute_stats(hc$treatment, hc$submit)
barplot2(p$p, p$se, ylim=c(0, 10)
  , border=gray(.75), col=gray(.75)
  , ylab="% employees with submissions"
  , xlab=NA)
dev.off()

pdf("figs/sorting.bar.pdf")

hc$office_y <- ifelse(hc$has_office=="yes","Fixed office", "Ward (no office)")
hc$gender2 <- factor(hc$gender)
levels(hc$gender2) <- capitalize(levels(hc$gender2))

par(mfrow=c(2,2), bty="n") 

barplot3(compute_stats(hc$gender2, hc$submit))
title("Gender")
barplot3(compute_stats(hc$job, hc$submit))
title("Profession")
barplot3(compute_stats(hc$office_y, hc$submit))
title("Office")

dev.off()


# 
# pdf("figs/part.bar.pdf")
# boxplot(num_ideas ~ treatment, hc, subset=num_ideas>0
#   , ylab="Project proposal in submission"
#   , xlab="Solicitation treatment")
# dev.off()

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