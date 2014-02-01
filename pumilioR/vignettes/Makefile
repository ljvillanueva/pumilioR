VIGS= setup.html

all: $(VIGS)

%.html: %.Rmd
	$(R_HOME)/bin/Rscript -e "knitr::knit2html('$*.Rmd', options = c('toc', markdown::markdownHTMLOptions(TRUE))"
