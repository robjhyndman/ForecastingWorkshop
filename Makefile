SOURCES=$(shell find . -name *.Rmd)
SOURCES := $(wildcard *.Rmd)
TARGETS=$(SOURCES:%.Rmd=%.pdf)

%.pdf: %.Rmd header.tex
	@echo "$< -> $@"
	@Rscript -e "rmarkdown::render('$<')"

default: $(TARGETS)

clean:
	rm -rfv $(TARGETS)
	latexmk -c
	rm -rf *_cache/
	rm -rf *_files/
	rm -rf Rfigs/*

deploy:
	rsync -zrvce 'ssh -p 18765' $(TARGETS) labs/*.csv labs/*.Rmd robjhynd@m80.siteground.biz:public_html/nyc2018
