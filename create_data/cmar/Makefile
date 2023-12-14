all:
	echo "author: do 'make docs install' and then 'cd ..; make (ETC)'; others: leave"
docs: force
	R --no-save -e 'library(rmarkdown); render("README.Rmd", "pdf_document")'
install: force
	cp *_cmar.csv ..
clean: force
	-rm *~ *.out
force:

