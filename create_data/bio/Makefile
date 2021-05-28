all:
	R --no-save -e 'library(rmarkdown); render("README.Rmd", "pdf_document")'
install: force
	cp *_bio.csv ..
clean: force
	-rm *~ *.out
force:

