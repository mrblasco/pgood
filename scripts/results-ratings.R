######################################## 
# This scripts compute tables and figures 
# for the Results section of the paper
# "Contributing to Public Goods"
######################################## 

######################################## 
## @knitr summary_ratings
######################################## 

submitters      <- hc$num_ideas>0
raters          <- hc$num_voted_ideas>0
ratings         <-  hc$num_voted_ideas
n_raters_group  <- sort(tapply(raters, hc$treatment, sum), decreasing=TRUE)

# Test association between proportion of raters 
# and solicitation treatments
ft <- fisher.test(table(hc$treatment, raters))

# Test association between volume of ratings 
# and solicitation treatments
l <- split(hc$num_voted_ideas[raters], hc$treatment[raters])
kt <- kruskal.test(l)

# Fraction of proponents voting own ideas
p_own_rated <- round(100*sum(hc$num_voted_ideas>0 & hc$num_ideas>0) / 60)


######################################## 
## @knitr table_ratings
######################################## 

# Ratings/raters by solicitation treatment
# (in appendix)
f <- function() {  
  text <- c("PRIZE", "FUND", "PCARE", "WPLACE")
  m <- table(hc$treatment, hc$num_voted_ideas>0)
  index <- match(text, rownames(m))
  m <- m[index, ]
  m <- rbind(m, "[1.8ex] Total"=apply(m, 2, sum))
  n <-  apply(m , 1, sum)
  p <- round(100*m[, 2] / n, 1)
  m <- cbind(m, p)
  proposals.sum <- tapply(hc$num_voted_ideas, hc$treatment, sum)
  proposals.ave <- tapply(hc$num_voted_ideas, hc$treatment, function(x)mean(x[x>0]))
  proposals.med <- tapply(hc$num_voted_ideas, hc$treatment, function(x)median(x[x>0]))
  index <- match(text, names(proposals.sum))
  proposals.sum <- c(proposals.sum[index], sum(proposals.sum))
  proposals.ave <- c(proposals.ave[index], mean(hc$num_voted_ideas[hc$num_voted_ideas>0]))
  proposals.med <- c(proposals.med[index], median(hc$num_voted_ideas[hc$num_voted_ideas>0]))
  m <- cbind(m, proposals.sum, proposals.ave, proposals.med)
  colnames(m) <- c("No", "Yes", "% yes", "Total", "Mean", "Median")
  return(m)
}
add <- rep()
add$cmd <- "& \\multicolumn{3}{c}{\\emph{Rating proposals:}} &         \\multicolumn{3}{c}{\\emph{Rated proposals:}} \\\\\n \\cmidrule(lr){2-4}\\cmidrule(lr){5-7}"
add$pos <- -1
table_render(f()
  , caption="Outcomes of the peer evaluation phase"
  , label="tab: ratings"
  , digits=c(0, 0, 0, 1, 0, 1, 0), add=add)

######################################## 
## @knitr table_drivers_of_ratings
######################################## 

fit <- rep()
fit$m1 <- lm(num_voted_ideas>0 ~ treatment, data=hc)
fit$m2 <- update(fit$m1, ~ job)
fit$m3 <- update(fit$m1, ~ gender)
fit$m4 <- update(fit$m1, ~ has_office)
fit$m6 <- update(fit$m1, ~ . + job + gender + has_office)
# fit$m5 <- update(fit$m1, ~ I(num_ideas>0))
# fit$m7 <- update(fit$m1, ~ . + I(num_ideas>0))
# fit$m8 <- update(fit$m1, ~ . + job + gender + has_office + I(num_ideas>0))

float(fit, coef=sapply(fit, function(x) 100*coef(x))
					, se=sapply(fit, function(x) 100*robust_se(x))
					, keep.stat="n", digits=1)


######################################## 
## @knitr figure_ratings
######################################## 

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
