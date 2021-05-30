all:
	echo "author: use 'make rda' and then 'make install'; others: leave"
rda: force
	Rscript create_rda.R
install: force
	cp mooringElements.rda ../data
clean: force
	-rm *~ *.out
force:

