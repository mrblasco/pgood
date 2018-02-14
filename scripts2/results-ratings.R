source("scripts/functions.R")
load("data-clean/mgh.RData")

######################################## 
## @knitr test_ratings
######################################## 

# TEST using Kruskal statistics
z <- aggregate(as.numeric(vote) ~ idea_id + treatment.proponent
							, data=voting, mean)
colnames(z)[3] <- "mean_rating"
l <- split(z[, 3], z[, 2])
kt <- kruskal.test(l) # Not significant

# TEST using OLS regression
ols <- lm(mean_rating ~ treatment.proponent, data=z)
ftest <- rep()
ftest$p.value <- anova(ols, test="F")[[4]][1]

# Disaggregated data
cs <- chisq.test(table(voting$vote, voting$treatment.proponent))

######################################## 
## @knitr content_project_proposals
######################################## 

m0 <- table(agg.voting$focus2, agg.voting$treatment.proponent)
ft <- fisher.test(m0, simulate.p.value=T, B=5e4)
cs <- chisq.test(m0, simulate.p.value=T, B=5e4)

######################################## 
## @knitr content_project_proposals_table
######################################## 

m <- addmargins(m0, 2)
colnames(m)[5] <- "Total"
m <- m[order(m[, 5], decreasing=TRUE), ]
m <- rbind(m, "[1.8ex] Total"=apply(m, 2, sum))
table_render(m, "Project proposals by area of focus", "tab: area-of-focus", digits=0, notes="The areas of focus were manually identified by executives at the end of the competition for all the project proposals (due to a technical problem five proposals ended up with no classification).")

######################################## 
## @knitr content_project_proposals_ols
######################################## 

# Regression for the areas of focus
fit <- rep()
for (l in unique(voting$focus2)) {
  voting$focus_patient <- voting$focus2==l
  agg <- aggregate(focus_patient ~ treatment.proponent + idea_id
  								, data=voting, unique)
  agg$treat2 <- factor(agg$treatment.proponent, 
                levels=c("WPLACE", "PCARE", "FUND", "PRIZE"))
  contrasts(agg$treat2) <-  named.contr.sum(levels(agg$treat2))
  fit[[l]] <- lm(focus_patient ~ treat2, data=agg)
}
ftest <- lapply(fit, anova, test="F")
pval <- sapply(ftest, function(x) x[1, 5])
m <- t(sapply(ftest, function(x) x[1, ]))
table_render(m[, 4:5]
		, "Areas of Focus and Solicitation Treatment"
		, "areas of focus"
		, digits=3)

######################################## 
## @knitr content_project_proposals_coef_plot
########################################

op <- par(family='serif')
y <- factor(agg.voting$focus2)
l <- factor(agg.voting$treatment)

mlev <- matrix(levels(hc$treatment)[combn(1:4, 2)], 6, byrow=TRUE)
ylev <- c("Care Coordination", "Staff workflow", "Workplace", "Information and access", "Patient support", "Quality and safety ")

xlim <- c(-.5, 0.4)
ylim <- c(1, 6) + c(-0.1, 0.1)

layout(matrix(c(1:6), 2, 3, byrow=T), width=c(1.25, 1, 1, 1.5, 1, 1))
for (k in 1:length(ylev)) {
  if (k%%3==1) {
    par(mar=c(2.1, 9.1, 5.1, 0.1), new=FALSE)
  } else {
    par(mar=c(2.1, 0.5, 5.1, 0.1), new=FALSE)  
  }
  plot(NA,NA, ylim=ylim, xlim=xlim, ann=FALSE, xaxt="n", yaxt="n", bty='n')
  for (i in 1:6) {
    index <- l %in% mlev[i, ]
    tab <- table(droplevels(l[index])
            , factor(y[index]==ylev[k], levels=c("FALSE", "TRUE")))
    tab <- tab + 1 
    n <- apply(tab, 1, sum)
    pbar <- tab[, 2] /  n 
    p.diff <- diff(-pbar)
    SE.diff <- sqrt(sum(pbar *(1-pbar) / n))
    add.error.bars(i, p.diff, SE.diff)
    points(y=i, x=p.diff, pch=16, cex=1.5)
    abline(v=0)
    mtext(ylev[k], 3, 3)
    x.seq <- seq(xlim[1], xlim[2], length=5)
    xticks <- pretty(x.seq)
    axis(3, at=xticks, paste(round(xticks*100), "%"))
    if (k%%3==1) {
        yticks <- 1:6
        axis(2, at=yticks, paste(mlev[, 1], "-",mlev[, 2]), las=2)
    } 
  }
}
