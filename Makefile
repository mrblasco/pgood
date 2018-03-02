# Makefile public good project
# Andrea Blasco 
# Fall 2017

# Tools
EDITOR := open -a BBEdit
VIEWER := open -a Skim

# Folders
TEXFILE := paper/report
RDIR := scripts
PAPERDIR := paper
FIGDIR := figs
TABDIR := tables
RFILES := $(wildcard $(RDIR)/*.R)
RMD_FILES:= $(wildcard $(PAPERDIR)/*.Rmd)
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
$(TEXFILE).pdf: $(OUT_FILES) $(RMD_FILES) $(PAPERDIR)/*.bib
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

# convert file into docx
docx:
	cd paper; pandoc report.tex --read=latex -t docx \
	  --filter pandoc-citeproc --bibliography refs.bib \
    --reference-doc misc/reference.docx \
    -o report.docx

# clean up
clean:
	 rm -fv $(OUT_FILES) 
	 rm -fv $(PDF_FILES)
	 rm -fv $(PNG_FILES)
	 rm -fv $(TEXFILE).pdf
	 mr -fv $(TAB_FILES)
 
.PHONY: all clean

