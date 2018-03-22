# experimental design table
source("scripts/lib/config.R")

# Treatment variable 
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
colnames(m) <- c("\t","N", "%")

add <- list(pos=-1)
add$cmd <- "& \\multicolumn{1}{c}{\\emph{Solicitation treatment:}}
						& \\multicolumn{2}{c}{\\emph{Employees:}}\\\\
						\\cmidrule(lr){2-2}\\cmidrule(lr){3-4}"

sink("tables/design.tex")
table_render(m, add=add
					, caption="Experimental design"
					, label="experimental-design"
					, align=c("@{}l", "p{6cm}>{\\raggedright}", "r", "r"))
sink()
