all:
	echo "Use 'make rda' and then 'make install'."
rda: force
	Rscript create_rda.R
install: force
	cp mooringElements.rda ../data
clean: force
	-rm *~ *.out
force:

