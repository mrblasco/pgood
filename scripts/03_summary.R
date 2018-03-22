# Summary Stats Table
source("scripts/lib/config.R")

# Outcome variables values
out <- c("Total employees solicited"=nrow(hc)
  , "% employees with submission"=percent(mean(hc$submit))
  , "Total employees with submission"=sum(hc$submit)
  , "Total submitted proposals"=sum(hc$num_ideas)
  , "% employees with evaluation"=percent(mean(hc$num_voted_ideas>0))
  , "Total ratings"=sum(hc$num_voted_ideas)
  , "Total employee finalists"=sum(hc$finalist)
  , "% finalists among those with submissions"=percent(sum(hc$finalist) / sum(hc$submit))
)

# additional rows for latex table
add <- list()
add$pos <- list(1,4,6)
add$cmd <- c("[1.86ex]\\emph{Submission phase:}&\\\\\n"
            , "[1.86ex]\\emph{Evaluation phase:}&\\\\\n"
            , "[1.86ex]\\emph{Implementation phase:}&\\\\\n")

sink("tables/summary.outcomes.tex")
table_render(data.frame(Value=out)
  , align=c("@{}p{8cm}", "r")
  , caption="Outcome variables"
  , label="outcomes"
  , digits=0, add=add)
sink()

###########################
message("Stop here -- TBA")
q() #######################
###########################

### FUNCTION ####

f <- function() {
  prepare.table <- function(tbl, TEST=chisq.test, ...) {
    tbl.test <- c(TEST(tbl, ...)$p.val, rep(NA, nrow(tbl)-1))
    m <- cbind(round(100*tbl/colSums(tbl), 1)
          , "%"=round(100*rowSums(tbl)/sum(rowSums(tbl)), 1)
          , "n"=rowSums(tbl)
          , "P-value"=round(tbl.test,3))
    return(m)
  }
  office <- ifelse(hc$has_office=="yes", "Office", "No office")
  age <- factor(hc$age, exclude=c("", NA))
  levels(age)[4:5] <- rep(">45", 2)
  tenure <- factor(round(hc$tenure/10))
  lv <- c("< 10", "10-20", "20-30", "30-40", ">40", ">40")
  levels(tenure) <- lv
  m <- rbind(prepare.table(table(hc$job, hc$treatment))
  , prepare.table(table(capitalize(hc$gender), hc$treatment))
  , prepare.table(table(office, hc$treatment))
  , prepare.table(table(age, hc$treatment), simulate.p=TRUE)
  , prepare.table(table(tenure, hc$treatment), simulate.p=TRUE))
  rownames(m)[rownames(m)=="female"] <- "[1.86ex] Female"
  rownames(m)[rownames(m)=="male"] <- "Male"
  rownames(m)[rownames(m)=="No office"] <- "[1.86ex] No office"
  colnames(m)[5:6] <- c('%', 'Obs.')
  return(m)
}

#### MAIN PROGRAM  ####

# test association b/w treatment and gender
cs <- chisq.test(table(hc$treatment, hc$gender))

# print table in latex
sink("tables/summary.tex")

add <- list()
add$pos <- list(-1, 7, 11)
add$cmd <- c("& \\multicolumn{4}{c}{\\emph{Assigned treatments:}} 
						& \\multicolumn{2}{c}{\\emph{All:}}\\\\
						\\cmidrule(lr){2-5}\\cmidrule(lr){6-7}"
						, "[1.86ex] Age* &&&&&&\\\\\n"
						, "[1.86ex] Tenure* &&&&&&\\\\\n")

table_render(f(), caption="Summary statistics by treatment"
            , label="summary-statistics", digits=c(rep(0, 7), 2), add=add
            , notes=sprintf("This table reports the percentage of employees in our sample cross tabulated by the assigned treatment across the gender, profession, whether the employee had a fixed office location, age, and years of tenure at the Heart Center. For each categorical variable, the last column reports the p-value from a %s with the assigned treatment and the variable. The asterisk $^{\\ast}$ indicates self-reported information obtained from an online survey polling employees about two months before the launch of the innovation contest.", cs$method))

sink()