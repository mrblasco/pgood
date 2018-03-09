source("scripts/lib/functions.R")
load("data-clean/mgh.RData")

# Voting data
str(voting) # 

# Test differences in proposal quality with non-parametric test
voting$vote_n <- as.numeric(voting$vote)
quality <- aggregate(vote_n ~ idea_id + treatment.proponent, voting, mean)
(quality.k.test <- kruskal.test(split(quality[, 3], quality[, 2])))
# p=0.45

# ... with parametric test using OLS
quality.ols <- lm(vote_n ~ treatment.proponent, quality)
(quality.ols.ftest <- anova(quality.ols, test="F"))
# p=0.6

# ... and chi-squared test 
quality.tab <- table(voting$vote, voting$treatment.proponent)
(quality.tab.cs.test <- chisq.test(quality.tab))
# p=0.10

# Content

# Create new category based on workplace/patient oriented interventions
# voting$focus3 <- factor(trimws(voting$focus2))
# lev0 <- levels(voting$focus3)
# index_f <- lev0 %in% c("Workplace", "Staff workflow", "Care Coordination")  
# lev0[index_f] <- "Workplace improvements"
# index_f <- lev0 %in% c("Information and access", "Patient support", "Quality and safety")  
# lev0[which(index_f)] <- "Patient care improvements"
# levels(voting$focus3) <- lev0


# Differences in the content
# Create cross section data about ideas
index <- tapply(1:nrow(voting), voting$idea_id, head, n=1)
voting.cs <- voting[index, ] 
(content.tab <- xtabs(~focus2 + treatment.proponent, voting.cs))

# Test differences in the content of a proposal 
(content.ftest <- fisher.test(content.tab, simulate.p.value=T, B=5e4)) 
# p=0.08
(content.cs.test <- chisq.test(content.tab, simulate.p.value=T, B=5e4))
# p=0.09

# plot results
content.tab.ord <- content.tab[order(rowSums(content.tab)), ]

# barplot
# par(mar=c(1,12,1,1))
# tab <- prop.table(content.tab.ord,2)
# barplot(tab, beside=F, hor=T, las=1, col=1:nrow(tab))

# Proportions
y <- apply(content.tab.ord, 2, function(x) (x) / sum(x))
message(paste(paste(letters[1:nrow(y)], rownames(y), sep="="), collapse=","))

# Line plot
pdf("figs/areas.pdf")
par(mfrow=c(2,2))
for (j in 1:ncol(y)) {
  plot(y[, j], lty=j, pch=j, lwd=2, type="b", ylim=c(0, 0.4)
    , ylab="% proposals", xlab="Areas of focus", bty="n", xaxt="n", yaxt="n")
  lines(y[,3], lty=4, pch=3, lwd=2, type="b", col=gray(.75))
  title(colnames(y)[j])
  axis(2, at= 1:10/10, 100*(1:10/10))
  axis(1, at= 1:nrow(y), letters[1:nrow(y)])
}
dev.off()

# Print table (APPENDIX)
sink("tables/content.tex")

Total <- function(x, ...) sum(x, ...)
m <- addmargins(content.tab, FUN=Total)
table_render(m, "Project proposals by area of focus", "tab: area-of-focus", digits=0, notes="The areas of focus were manually identified by executives at the end of the competition for all the project proposals (due to a technical problem five proposals ended up with no classification).")

sink()

# QUIT FROM HERE ON
q()

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

# op <- par(family='serif')
# y <- factor(agg.voting$focus2)
# l <- factor(agg.voting$treatment)
# 
# mlev <- matrix(levels(hc$treatment)[combn(1:4, 2)], 6, byrow=TRUE)
# ylev <- c("Care Coordination", "Staff workflow", "Workplace", "Information and access", "Patient support", "Quality and safety ")
# 
# xlim <- c(-.5, 0.4)
# ylim <- c(1, 6) + c(-0.1, 0.1)
# 
# layout(matrix(c(1:6), 2, 3, byrow=T), width=c(1.25, 1, 1, 1.5, 1, 1))
# for (k in 1:length(ylev)) {
#   if (k%%3==1) {
#     par(mar=c(2.1, 9.1, 5.1, 0.1), new=FALSE)
#   } else {
#     par(mar=c(2.1, 0.5, 5.1, 0.1), new=FALSE)  
#   }
#   plot(NA,NA, ylim=ylim, xlim=xlim, ann=FALSE, xaxt="n", yaxt="n", bty='n')
#   for (i in 1:6) {
#     index <- l %in% mlev[i, ]
#     tab <- table(droplevels(l[index])
#             , factor(y[index]==ylev[k], levels=c("FALSE", "TRUE")))
#     tab <- tab + 1 
#     n <- apply(tab, 1, sum)
#     pbar <- tab[, 2] /  n 
#     p.diff <- diff(-pbar)
#     SE.diff <- sqrt(sum(pbar *(1-pbar) / n))
#     add.error.bars(i, p.diff, SE.diff)
#     points(y=i, x=p.diff, pch=16, cex=1.5)
#     abline(v=0)
#     mtext(ylev[k], 3, 3)
#     x.seq <- seq(xlim[1], xlim[2], length=5)
#     xticks <- pretty(x.seq)
#     axis(3, at=xticks, paste(round(xticks*100), "%"))
#     if (k%%3==1) {
#         yticks <- 1:6
#         axis(2, at=yticks, paste(mlev[, 1], "-",mlev[, 2]), las=2)
#     } 
#   }
# }
