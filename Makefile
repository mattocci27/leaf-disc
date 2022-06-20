GIT = 36a960d # second submission
all: ms/LMA_method.pdf ms/LMA_method.docx ms/LMA_method-diff$(GIT).pdf ms/response_letter_2.pdf
#docker: ms/LMA_method.pdf ms/diff.pdf
local: ms/leaf_disc.bib
dif: ms/LMA_method-diff$(GIT).pdf

ms/LMA_method.pdf: ms/LMA_method.Rmd values.yml figs/* ms/leaf_disc.bib
	R -e 'system.time(rmarkdown::render("$<", "bookdown::pdf_document2"))'

ms/LMA_method.docx: ms/LMA_method.Rmd values.yml figs/* ms/leaf_disc.bib
	R -e 'system.time(rmarkdown::render("$<", "bookdown::word_document2"))'

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

# ms/leaf_disc.bib: ~/trait-method.bib
# 	cp $< $@

ms/response_letter.pdf: ms/response_letter.Rmd
	R -e 'system.time(rmarkdown::render("$<", "all"))'

ms/response_letter.docx: ms/response_letter.Rmd
	R -e 'system.time(rmarkdown::render("$<", "all"))'

ms/response_letter_2.pdf: ms/response_letter_2.Rmd
	R -e 'system.time(rmarkdown::render("$<", "pdf_document"))'

.PHONY: clean
clean:
	rm -f ms/*.tuc \
	ms/*.log \
	ms/*.aux\
	ms/*.fdb_* \
	ms/*.gz \
	ms/*.fls
	rm -rf docs/components
