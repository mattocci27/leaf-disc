GIT = 36a960d # second submission
all: ms/LMA_method.pdf ms/LMA_method-diff$(GIT).pdf ms/SI.pdf
docker: ms/LMA_method.pdf
#docker: ms/LMA_method.pdf ms/diff.pdf
local: ms/leaf_disc.bib
dif: ms/diff.pdf

ms/LMA_method.pdf: ms/LMA_method.Rmd values.yml figs/*
	R -e 'system.time(rmarkdown::render("$<", "all"))'

ms/LMA_method-diff$(GIT).pdf: ms/LMA_method.tex
	latexdiff-vc --git --flatten --force -r $(GIT) $^ ; \
  cd ms; pdflatex LMA_method-diff$(GIT).tex

ms/cover.pdf: ms/cover.md
	pandoc $< \
	--template ms/templates/eisvogel2.tex \
	-o $@

ms/cover.docx: ms/cover.md
	pandoc $< \
  --reference-doc ms/templates/cover_style.docx \
	-o $@

ms/leaf_disc.bib: ~/trait-method.bib
	cp $< $@

ms/SI.pdf: ms/SI.Rmd
	R -e 'system.time(rmarkdown::render("$<", "all"))'

ms/response_letter.pdf: ms/response_letter.Rmd
	R -e 'system.time(rmarkdown::render("$<", "all"))'

ms/response_letter.docx: ms/response_letter.Rmd
	R -e 'system.time(rmarkdown::render("$<", "all"))'

.PHONY: clean
clean:
	rm -f ms/*.tuc \
	ms/*.log \
	ms/*.aux\
	ms/*.fdb_* \
	ms/*.gz \
	ms/*.fls
	rm -rf docs/components
