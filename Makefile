# Makefile public good project
# Andrea Blasco 
# Fall 2017

# Tools
EDITOR := open -a BBEdit
VIEWER := open -a Skim

# Folders
TEXFILE := paper/report
RDIR := scripts2
PAPERDIR := paper
FIGDIR := figs
TABDIR := tables
RFILES := $(wildcard $(RDIR)/*.R)
DOC_FILES:= $(wildcard $(PAPERDIR)/*.Rmd)
PDF_FILES := $(wildcard $(FIGDIR)/*.pdf)
TAB_FILES :=  $(wildcard $(TABDIR)/*.tex)
OUT_FILES:=  $(RFILES:%.R=%.Rout)
PNG_FILES :=  $(PDF_FILES:%.pdf=%.png)

all: $(OUT_FILES) $(TEXFILE).pdf $(PNG_FILES)

# run every R file, when needed
$(RDIR)/%.Rout: $(RDIR)/%.R
	R CMD BATCH $< $@

# convert PDF to PNG
$(FIGDIR)/%.png: $(FIGDIR)/%.pdf
	sips -s format png $< --out $@

# compile main tex file
$(TEXFILE).pdf: $(OUT_FILES) $(DOC_FILES) $(PAPERDIR)/*.bib
	Rscript -e 'rmarkdown::render("paper/report.Rmd");'

# produce all ROUT files
R: $(OUT_FILES)

# produce all PNG files
png: $(PNG_FILES)

# view report
view: $(TEXFILE).pdf
	$(VIEWER) $(TEXFILE).pdf & 

# edit bibliography
bib: 
	$(EDITOR) paper/*.bib 

# clean up
clean:
	 rm -fv $(OUT_FILES) 
	 rm -fv $(PDF_FILES)
	 rm -fv $(PNG_FILES)
	 rm -fv $(TEXFILE).pdf
	 mr -fv $(TAB_FILES)
 
.PHONY: all clean


############################
# VIEWER := open -a Skim
# CONFIG := config.R
# RUN := R CMD BATCH
# RDIR := scripts2
# RFILES := $(wildcard $(RDIR)/*.R)
# OUT_FILES:= $(RFILES:.R=.Rout)
# #########################


# knit the paper with rmarkdown
# paper: 
#   Rscript -e 'rmarkdown::render("paper/report.Rmd");'

