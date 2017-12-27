#!/usr/bin/env Rscript
######################################## 
# This scripts contains R chunks 
#
# --- methods_timeline
# --- methods_summary_stats
# --- methods_experimental_design
######################################## 


######################################## 
## @knitr methods_timeline
######################################## 

# Plots timeline of the experiment
plot_timeline <- function(x, ...) {
	d <- as.Date(x$date)
	a <- string.break(x$activity)
	par(mar=c(4.1,2.1,4.1,2.1))
	xlim <- range(c(d+15, d-5))
	plot(d, rep(1,7), pch=16, bty="n", yaxt="n", xaxt="n", ann=FALSE
	  , ylim=c(-2, 2), xlim=xlim, ...)
	yrand <- c(-1.4, -0.8, 2, 1.5, -1.8, 1.5, 1.5)
	text(x=d, y=yrand, a, pos=4, xpd=TRUE)
	segments(x0=d, y0=1, y1=yrand)
	segments(x0=d, x1=d+1, y0=yrand, y1=yrand)
	abline(h=1)
	x.sq <- as.Date(c("2014-08-01", "2014-09-01", "2014-10-01", "2014-11-01"))
	text(x=x.sq, y=1, format(x.sq, "%b"),pos=1)
	points(x=x.sq, y=rep(1, 4), pch=3)
}

plot_timeline(x=read.csv("timing.csv"))


######################################## 
## @knitr methods_experimental_design
######################################## 

design <- function() {
p <- c("Submit your ideas to win an Apple iPad mini"
	, "Submit your ideas to win project funding up to $20,000 
			to turn your ideas into actions"
	, "Submit your ideas to improve patient care at the Heart Center"
	, "Submit your ideas to improve the workplace at the Heart Center")
	txt <- data.frame(treatment=c("PRIZE","FUND", "PCARE", "WPLACE"), paragraph=p)
	m <- table(hc$treatment)
	index <- match(txt$treatment, names(m))
	m <- m[index]
	m <- cbind(m, round(100*m/sum(m)))
	rownames(m)[-1] <- paste("[1.8ex]", rownames(m)[-1])
	m <- rbind(m, "[1.8ex] Total"=apply(m, 2, sum))
	m <- cbind(c(as.character(txt$paragraph), NA), m)
	colnames(m) <- c(".","freq.", "%")
	return(m)
}
add <- list(pos=-1)
add$cmd <- "& \\multicolumn{1}{c}{\\emph{Solicitation treatment:}}
						& \\multicolumn{2}{c}{\\emph{Employees:}}\\\\
						\\cmidrule(lr){2-2}\\cmidrule(lr){3-4}"
table_render(design(), caption="Experimental design", label="experimental-design", add=add, align=c("@{}l", "p{5cm}", "r", "r"))


######################################## 
## @knitr methods_summary_stats
######################################## 

n_survey <- ""

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
  levels(age) <- paste(levels(age), "years old*")
  levels(age)[4:5] <- rep(">45 years old*", 2)
  tenure <- factor(round(hc$tenure/10))
  lv <- c("< 10", "10-20", "20-30", "30-40", ">40", ">40")
  levels(tenure) <- paste(lv, "years tenure*")

  m <- rbind(prepare.table(table(hc$job, hc$treatment))
  , prepare.table(table(capitalize(hc$gender), hc$treatment))
  , prepare.table(table(office, hc$treatment))
  , prepare.table(table(age, hc$treatment), simulate.p=TRUE)
  , prepare.table(table(tenure, hc$treatment), simulate.p=TRUE))
  rownames(m)[rownames(m)=="Female"] <- "[1.86ex] Female"
  rownames(m)[rownames(m)=="No office"] <- "[1.86ex] No office"
  rownames(m)[rownames(m)=="18-25 years old*"] <- "[1.86ex] 18-25 years old*"
  rownames(m)[rownames(m)=="< 10 years tenure*"] <- "[1.86ex] < 10 years tenure*"
  colnames(m)[5:6] <- c('%', 'Obs.')
  return(m)
}
cs <- chisq.test(table(hc$treatment, hc$gender))
add <- list(pos=-1)
add$cmd <- "& \\multicolumn{4}{c}{\\emph{Assigned treatments:}} 
						& \\multicolumn{2}{c}{\\emph{All:}}\\\\
						\\cmidrule(lr){2-5}\\cmidrule(lr){6-7}"
table_render(f(), caption="Summary statistics by treatment"
            , label="summary-statistics", digits=c(rep(0, 7), 3), add=add
            , notes=sprintf("This table reports the percentage of employees in our sample cross tabulated by the assigned treatment across the gender, profession, whether the employee had a fixed office location, age, and years of tenure at the Heart Center. For each categorical variable, the last column reports the p-value from a %s with the assigned treatment and the variable. The asterisk $^{\\ast}$ indicates non-representative self-reported information obtained from an online survey polling %s employees that was run about two months before the launch of the innovation contest.", cs$method, n_survey))
