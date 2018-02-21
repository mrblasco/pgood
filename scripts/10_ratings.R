source("scripts/functions.R")
load("data-clean/mgh.RData")


# Ratings/raters by solicitation treatment
# (in appendix)
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