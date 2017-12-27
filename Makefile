PAPERDIR=paper

all: paper.Rout

# Compile markdown file
paper.Rout: $(PAPERDIR)/report.Rmd
	Rscript -e 'rmarkdown::render("$<");' > paper.Rout

# Dependencies
paper.Rout: $(addprefix $(PAPERDIR)/,*.Rmd refs.bib R/*.R)


# Slides
slides.Rout: slides/slides.Rmd
		Rscript -e 'rmarkdown::render("$<");' > slides.Rout

# View
view:
	open -a Skim $(PAPERDIR)/report.pdf

view-slides:
	open -a Skim slides/slides.pdf
	
# Clean files
clean:
	rm -fv *.Rout 
