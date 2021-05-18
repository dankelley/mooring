all:
	echo "Use 'make rda' and then 'make install'. (The author did 'make csv' but that should not need repeating.)"
csv: force
	Rscript create_csv.R
rda: force
	Rscript create_rda.R
install: force
	cp mooringElements.rda ../data
clean: force
	-rm *~ *.out
force:

