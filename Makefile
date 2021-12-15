CV = data/full_data_cv.csv data/tree_data.csv docs/data_clean_cv.html

all: data/full_data.csv $(CV) values.yml docs/figs.html ms/leaf_disc.bib ms/LMA_method.pdf ms/diff.pdf ms/SI.html ms/cover.pdf ms/cover.docx

docker: data/full_data.csv $(CV) values.yml docs/figs.html ms/LMA_method.pdf ms/diff.pdf ms/SI.html ms/cover.pdf ms/cover.docx

analysis: data/full_data.csv $(CV) values.yml docs/figs.html

pandoc: ms/LMA_method.pdf ms/diff.pdf ms/SI.html ms/cover.pdf ms/cover.docx

data/full_data.csv: docs/data_clean.Rmd
	R -e 'system.time(rmarkdown::render("$<", "all"))'

$(CV): emptytarget1
emptytarget1: docs/data_clean_cv.Rmd
	R -e 'system.time(rmarkdown::render("$<", "all"))'
	touch $@

docs/figs.html: docs/figs.Rmd
	R -e 'system.time(rmarkdown::render("$<", "all"))'

values.yml: scripts/yml.R data/full_data.csv
	Rscript $<

ms/LMA_method.pdf: ms/LMA_method.Rmd values.yml ms/leaf_disc.bib figs/*
	R -e 'system.time(rmarkdown::render("$<", "all"))'

ms/SI.html: ms/SI.Rmd values.yml
	R -e 'system.time(rmarkdown::render("$<", "all"))'

ms/diff.pdf: ms/LMA_method_old.tex ms/LMA_method.tex
	latexdiff ms/LMA_method_old.tex ms/LMA_method.tex > ms/diff.tex
	cd ms; pdflatex diff.tex

ms/cover.pdf: ms/cover.md
	pandoc $< \
	--template ms/templates/eisvogel2.tex \
	-o $@

ms/cover.docx: ms/cover.md
	pandoc $< \
        --reference-doc ms/templates/cover_style.docx \
	-o $@

ms/leaf_disc.bib: ~/trait_method.bib
	cp $< ./ms/

.PHONY: clean
clean:
	rm -f ms/*.tuc \
	ms/*.log
	rm -rf docs/components
