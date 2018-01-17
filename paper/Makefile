VIEWER=open -a Skim
EDITOR=open -a BBEdit
SRC := scripts

# Paper
report.pdf: report.Rmd
	Rscript -e 'rmarkdown::render("$<");' > report.Rout

# Paper dependencies
report.pdf: *.Rmd refs.bib template.tex $(SRC)/*.R

# View
view:
	$(VIEWER) report.pdf

# Edit
edit: 
	$(EDITOR) *.Rmd *.bib makefile $(SRC)/*.R

# Clean files
clean:
	rm -fv *.Rout *.pdf *.bbl *.ttt *.fff
