all: README.html read_dewey.out example_01.out knockdown_model.pdf object_orientation.pdf
README.html: README.md
	pandoc $< > $@
knockdown_model.pdf: knockdown_model.Rmd
	R --no-save -e 'library(rmarkdown); render("'$<'", "pdf_document")'
object_orientation.pdf: object_orientation.Rmd
	R --no-save -e 'library(rmarkdown); render("'$<'", "pdf_document")'
%.out: %.R
	R --no-save < $< > $@
clean: force
	-rm model.pdf *.out *~
force:
