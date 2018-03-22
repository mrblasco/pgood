# Libraries
source("scripts/lib/config.R")

# Rating
tab <- xtabs(~ifelse(num_voted_ideas>0,"Yes","No")+treatment, data=hc)
tab.count <- apply(tab, 2, function(x) paste("(", x, ")", sep=""))
tab.pc <- apply(percent(prop.table(tab, 2)), 2, function(x) paste(x, "%"))
tab.final <- rbind(tab.pc, tab.count)[c(1,3,2,4), ]
labels <- c("% making no evaluations", "", "% making evaluations", "")
out <- data.frame(labels, tab.final)

# additional rows for latex table
add <- list()
add$pos <- list(-1)
add$cmd <- c("& \\multicolumn{4}{c}{\\emph{Solicitation treatment:}}\\\\
						\\cmidrule(lr){2-5} \\emph{Peer evaluation:} &")

sink("tables/ratings_by_treatment.tex")
table_render(out
  , align=c("@{}l","@{}l", rep("c", ncol(out)-1))
  , caption="Employee participation in the peer evaluation phase"
  , label="tab: ratings"
  , add=add
  , digits=0
  , include.rownames=FALSE
  , sanitize.colnames.function=function(x)x[-1])
sink()


# Ratings/raters by solicitation treatment
sink("tables/ratings.tex")

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

sink()