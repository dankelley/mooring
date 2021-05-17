all: example_01.out knockdown_model.pdf object_orientation.pdf
one_time: read_dewey.out
knockdown_model.pdf: knockdown_model.Rmd
	R --no-save -e 'library(rmarkdown); render("'$<'", "pdf_document")'
object_orientation.pdf: object_orientation.Rmd
	R --no-save -e 'library(rmarkdown); render("'$<'", "pdf_document")'
%.out: %.R
	R --no-save < $< > $@
clean: force
	-rm model.pdf *.out *~
force:
