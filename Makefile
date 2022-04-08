all: ms/LMA_method.pdf
docker: ms/LMA_method.pdf ms/diff.pdf

ms/LMA_method.pdf: ms/LMA_method.Rmd values.yml figs/*
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

ms/response_letter.pdf: ms/response_letter.Rmd
	R -e 'system.time(rmarkdown::render("$<", "all"))'
#	pandoc $< \
	--template ms/templates/eisvogel2.tex \
	-o $@

ms/response_letter.docx: ms/response_letter.Rmd
	R -e 'system.time(rmarkdown::render("$<", "all"))'
# pandoc $< \
	-o $@

.PHONY: clean
clean:
	rm -f ms/*.tuc \
	ms/*.log \
	ms/*.aux \
	ms/*.fdb_* \
	ms/*.gz
	rm -rf docs/components
