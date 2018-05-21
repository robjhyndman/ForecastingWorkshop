SOURCES=$(shell find . -name *.Rmd)
SOURCES := $(wildcard *.Rmd)
TARGETS=$(SOURCES:%.Rmd=%.pdf)

%.pdf: %.Rmd
	@echo "$< -> $@"
	@Rscript -e "rmarkdown::render('$<')"

default: $(TARGETS)

latex:
	latexmk 3-3-Hierarchical.tex
	latexmk 3-5-Wrapup.tex

update:
	cp -uf 1-*.pdf ../Day1/
	cp -uf 2-*.pdf ../Day2/
	cp -uf 3-*.pdf ../Day3/
	cp -uf 1-*.pdf ~/git/Website/static/uwa2017/
	cp -uf 2-*.pdf ~/git/Website/static/uwa2017/
	cp -uf 3-*.pdf ~/git/Website/static/uwa2017/

clean:
	rm -rf $(TARGETS)
	latexmk -c
	