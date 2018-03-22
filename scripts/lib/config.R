require(xtable)

# Load data
load("data/mgh.RData")

# Define new vars
hc$submit <- hc$num_ideas>0


#### Define Helper Functions  ####
percent <- function(x, ...) {
	round(100 * x, ...)
}
string.break <- function(x) {
  gsub('(.{1,12})(\\s|$)', '\\1\n', x)
}

# Function to compute stats from a table with x and y;
# it returns a list with proportion, SE, n, and CI 
# for x and y
compute_stats <- function(x, y) {
	tab <- table(x, y)
	ptab <- prop.table(tab, 1)
	p <- 100*ptab[, 2]
	n <- apply(tab, 1, sum)
	se <- sqrt(p*(100-p)/n)
	lab <- format(p, digits=2)
	return(list(p=p,n=n,se=se,labels=lab))
}

# Function to plot the computed stats as a barplot
plot_bars <- function(p, se, labels, ...) {
	b <- barplot(p, las=1, ...)
	arrows(x0=b, y0=p, y1=p+sign(p)*se, angle=90, length=0.1)
}

# Function to compute robust standard errors in ols
robust_se <- function (object) {
    require(sandwich)
    stopifnot(class(object) %in% c("lm", "glm"))
    vcov.mat <- vcovHC(object)
    sqrt(diag(vcov.mat))
}

# Float in latex
float <- function (x, caption=NULL, label=NULL, notes=NULL, notes.width="\\textwidth", ...) {
  cat('\\begin{table}\n\\centering\n')  
  cat(sprintf('\\caption{%s}', caption))
  cat(sprintf('\\label{%s}', label))
  tab <- capture.output(stargazer(x, header=FALSE, float=FALSE, ...))
  index <- grep("Note:", tab)
  cat(tab[-index], sep='\n')
  if (!is.null(notes)) {
    cat(sprintf('\\begin{minipage}{%s}\n', notes.width))
    cat(sprintf('\\emph{Note:} %s\n', notes))
    cat('\\end{minipage}\n')
  }
  cat('\\end{table}\n')  
}


# Coef plot
coef.plot <- function(pbar, SE, xlim=NULL, ylim=NULL, labels=NULL, ...) {
  if (is.null(xlim) | is.null(ylim)) { 
    xlim <- range(pbar + 2*SE, pbar - 2*SE)
    ylim <- c(1, length(pbar))
  }
  yticks <- length(pbar):1
  if (!is.null(labels)) par(mar=c(2.1, 9.1, 4.1, 2.1))
  plot(y=yticks, x=pbar, ylim=ylim, xlim=xlim
    , pch=16, ann=FALSE, xaxt="n", yaxt="n", bty="n")
  add.error.bars(yticks, pbar, SE, ...)
  abline(v=0)
  # X axis 
  xticks <- pretty(seq(xlim[1], xlim[2], length=5))
  axis(3, at=xticks, paste(round(xticks*100), "%", sep=''))
  # Y Axis
  if (!is.null(labels)) axis(2, at=yticks, labels, las=2)
  else text(y=(yticks)-0.02, x=pbar, names(pbar), pos=1, xpd=TRUE)
}
add.error.bars <- function(y, p, SE, alpha.levels=c(0.316, 0.10, 0.05), ...) {
    for (a in alpha.levels) {
      w <- qnorm(1 - a/2)
      i <- which(a==alpha.levels)
      lwd <- ifelse(i==1, 4.5, ifelse(i==2, 3, 1))
      segments(y0=y, x0=p + w*SE, x1=p-w*SE, lwd=lwd, ...)
    }
}


# Function to render tables into latex
table_render <- function(x, caption, label, add=NULL, align=NULL, digits=1, notes=NULL, ...) {
	require(xtable)
  if (is.null(align)) align <- c("@{}l",rep("c", ncol(x)))
  table.head <- "\\\\[-1.8ex]\\hline \\hline \\\\[-1.8ex]\n"
  table.mid <- "\\hline \\\\[-1.86ex]\n"
  table.bottom <- table.head
  add.to.row <- list()
  add.to.row$pos <- list(-1, 0, nrow(x))
  add.to.row$command <- c(table.head, table.mid, table.bottom)
  if (!is.null(add)) {
    add.to.row$command  <- c(add.to.row$command, add$cmd)
    for (k in 1:length(add$pos)) {
    	add.to.row$pos[[k+3]] <- add$pos[[k]]
    }
  }
  cat("\\begin{table}\n")         
  cat("\\centering\n")                  
  cat(sprintf("\\caption{%s}\n", caption))
  cat(sprintf("\\label{%s}\n", label))
  print(xtable(x, caption, label, align, digits)
      , add.to.row=add.to.row
      , hline.after=NULL
      , floating=FALSE
      , comment=FALSE, ...)
  if (!is.null(notes)) {
    cat("\\begin{minipage}{\\textwidth}\\itshape\\footnotesize\n")                  
    cat(sprintf("Note: %s\n", notes))
    cat("\\end{minipage}\n")
  }
  cat("\\end{table}\n")
}   


# Render tables
# table_render <- function(x, caption, label, add=NULL, align=NULL, digits=1, notes=NULL, ...) {
#   if (is.null(align)) align <- c("@{}l",rep("c", ncol(x)))
#   table.head <- "\\\\[-1.8ex]\\hline \\hline \\\\[-1.8ex]\n"
#   table.mid <- "\\hline \\\\[-1.86ex]\n"
#   table.bottom <- table.head
#   add.to.cmd <- c(table.head, table.mid, table.bottom)
#   add.to.row <- list(pos = list(-1, 0, nrow(x))
#                 , command = add.to.cmd)
#   if (!is.null(add)) {
#     add.to.row$command  <- c(add.to.row$command, add$cmd)
#     for (i in 1:length(add$pos)) {
#       add.to.row$pos[[3+i]] <- add$pos[[i]]
#     }
#   }
#   cat("\\begin{table}\n")   
#   cat("\\centering\n")                  
#   cat(sprintf("\\caption{%s}\n", caption))
#   cat(sprintf("\\label{%s}\n", label))
#   print(xtable(x, caption, label, align, digits)
#       , add.to.row=add.to.row
#       , hline.after=NULL
#       , floating=FALSE
#       , comment=FALSE
#       , ...)
#   if (!is.null(notes)) {
#     cat("\\begin{tablenotes}\n")                  
#     cat(sprintf("%s\n", notes))
#     cat("\\end{tablenotes}\n")
#   }
#   cat("\\end{table}\n")
# }  