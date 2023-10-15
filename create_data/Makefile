all:
	echo "author: use 'make rda install'; others: please do not use this Makefile"
rda: force
	Rscript create_rda.R
install: force
	cp mooringElements.rda ../data
clean: force
	-rm *~ *.out
force:

