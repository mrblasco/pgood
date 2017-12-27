PAPERDIR=paper
VIEWER=open -a Skim

# Paper
paper.Rout: $(PAPERDIR)/report.Rmd
	Rscript -e 'rmarkdown::render("$<");' > paper.Rout

# Paper dependencies
paper.Rout: $(addprefix $(PAPERDIR)/,*.Rmd refs.bib R/*.R)

# Slides
slides.Rout: slides/slides.Rmd
		Rscript -e 'rmarkdown::render("$<");' > slides.Rout

# View
view:
	$(VIEWER) $(PAPERDIR)/report.pdf

view-slides:
	$(VIEWER) slides/slides.pdf
	
# Clean files
clean:
	rm -fv *.Rout
