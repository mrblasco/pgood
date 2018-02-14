# Compute pairwise difference of proportions
source("scripts/functions.R")
load("data-clean/mgh.RData")

#### MAIN PROGRAM ###############

# examine all pairwise comparisons
pairwise.diff <- pairwise.prop.test(table(hc$treatment, !hc$num_ideas>0), p.adjust.method="none", correct=FALSE)

# Adj. p-values for false discovery rate
pairwise.diff.fdr <- pairwise.prop.test(table(hc$treatment, !hc$num_ideas>0), p.adjust.method="fdr", correct=FALSE)
pairwise.diff.fdr 


# print as latex table
sink("tables/pairwise.tex")
table_render(pairwise.diff$p.val
						, digits=3
						, caption="P-values for pairwise comparison of proportions"
						, label="pairwise"
						, notes="This table reports the p-values of pairwise comparisons of proportions
						         among solicitation treatments.")
sink()


# print as latex table
sink("tables/pairwise.fdr.tex")
table_render(pairwise.diff.fdr$p.val
						, digits=3
						, caption="Pairwise comparison of proportions"
						, label="pairwise.fdr"
						, notes="This table reports the p-values (adjusted for the false-discovery rate)
						        of pairwise comparisons of proportions among solicitation treatments.")
sink()
