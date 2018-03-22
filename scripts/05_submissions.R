# Submission rates
source("scripts/lib/config.R")

#### FUNCTIONS ####################

#  function computes submission differences
f <- function() {
  text <- c("PRIZE", "FUND", "PCARE", "WPLACE")
  m <- table(hc$treatment, hc$num_ideas>0)
  index <- match(text, rownames(m))
  m <- m[index, ]
  m <- rbind(m, "[1.8ex] Total"=apply(m, 2, sum))
  n <-  apply(m , 1, sum)
  p <- round(100*m[, 2] / n, 1)
  m <- cbind(m, p)
  nsub      <- tapply(hc$num_ideas, hc$treatment, sum)
  nsub.med  <- tapply(hc$num_ideas, hc$treatment, function(x) median(x[x>0]))
  nsub.mean  <- tapply(hc$num_ideas, hc$treatment, function(x) mean(x[x>0]))
  index <- match(text, names(nsub))
  nsub <- c(nsub[index], sum(nsub))
  nsub.med <- c(nsub.med[index], median(hc$num_ideas[hc$num_ideas>0]))
  nsub.mean <- c(nsub.mean[index], mean(hc$num_ideas[hc$num_ideas>0]))
  m <- cbind(m, nsub, nsub.mean, nsub.med)
  colnames(m) <- c("No", "Yes", "% yes", "Total", "Mean", "Median")
  return(m)
}


##### MAIN PROGRAM ################

# compute table
submission.table <- f()

# print table in latex
sink("tables/submissions.tex")

add <- rep()
add$cmd <- "& \\multicolumn{3}{c}{\\emph{Submitting proposals:}}
            & \\multicolumn{3}{c}{\\emph{Submitted proposals:}} \\\\
            \\cmidrule(lr){2-4}\\cmidrule(lr){5-7}"
add$pos <- -1
table_render(submission.table
  , caption="Outcomes of the submission phase"
  , label="tab: submissions"
  , digits=c(0, 0, 0, 1, 0, 1, 0)
  , add=add)

sink()