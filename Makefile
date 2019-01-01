REPORTS = theory/multi_norm.html theory/nb_counts.html theory/single_norm.html

all: $(REPORTS)

$(REPORTS): %.html: %.Rmd
	cd $(shell dirname $<) && R --no-save --slave -e 'rmarkdown::render(basename("$<"))'

clean:
	rm -f $(REPORTS)
