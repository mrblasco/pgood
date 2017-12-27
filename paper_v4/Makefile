# Compile markdown file
out/compile.Rout: R/compile.R
	Rscript R/compile.R > out/compile.Rout

# File dependencies
out/compile.Rout: *.Rmd refs.bib R/*.R

# View report
view: report.pdf
	open -a Skim report.pdf &

# Clean files
clean:
	rm -fv out/*.Rout 