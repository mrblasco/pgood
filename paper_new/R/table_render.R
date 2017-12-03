# Function to render tables into latex

table_render <- function(x, caption, label, add=NULL, align=NULL, digits=1, notes=NULL) {
	require(xtable)
  if (is.null(align)) align <- c("@{}l",rep("c", ncol(x)))
  table.head <- "\\\\[-1.8ex]\\hline \\hline \\\\[-1.8ex]\n"
  table.mid <- "\\hline \\\\[-1.86ex]\n"
  table.bottom <- table.head
  add.to.cmd <- c(table.head, table.mid, table.bottom)
  add.to.row <- list(pos = list(-1, 0, nrow(x))
                , command = add.to.cmd)
  if (!is.null(add)) {
    add.to.row$command  <- c(add.to.row$command, add$cmd)
    add.to.row$pos[[4]] <- add$pos
  }
  cat("\\begin{table}\n")         
  cat("\\centering\n")                  
  cat(sprintf("\\caption{%s}\n", caption))
  cat(sprintf("\\label{%s}\n", label))
  print(xtable(x, caption, label, align, digits)
      , add.to.row=add.to.row
      , hline.after=NULL
      , floating=FALSE
      , comment=FALSE)
  if (!is.null(notes)) {
    cat("\\begin{tablenotes}\n")                  
    cat(sprintf("%s\n", notes))
    cat("\\end{tablenotes}\n")
  }
  cat("\\end{table}\n")
}           
