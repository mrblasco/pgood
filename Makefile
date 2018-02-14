# Makefile public good project
# Andrea Blasco 
# Fall 2017

TEXFILE := paper/report
RDIR := scripts2
FIGDIR := figs
RFILES := $(wildcard $(RDIR)/*.R)
OUT_FILES:=  $(RFILES:%.R=%.Rout)

all: $(OUT_FILES) $(TEXFILE).pdf

# run every R file, when needed
$(RDIR)/%.Rout: $(RDIR)/%.R
	R CMD BATCH $< $@

# Compile main tex file and show errors
$(TEXFILE).pdf: $(OUT_FILES)
	Rscript -e 'rmarkdown::render("paper/report.Rmd");'

# Run R files
R: $(OUT_FILES)

# View main tex file
# view: $(TEXFILE).pdf
#     evince $(TEXFILE).pdf &

# Clean up stray files
# clean:
#     rm -fv $(OUT_FILES) 
#     rm -fv $(CROP_FILES)
#     rm -fv *.aux *.log *.toc *.blg *.bbl *.synctex.gz
#     rm -fv *.out *.bcf *blx.bib *.run.xml
#     rm -fv *.fdb_latexmk *.fls
#     rm -fv $(TEXFILE).pdf
# 
# .PHONY: all clean


############################
# EDITOR := open -a BBEdit
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

