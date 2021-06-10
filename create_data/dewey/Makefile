all:
	echo "author: do 'make csv install'; others: please do not use 'make'"
csv: force
	Rscript create_dewey_csv.R
install: force
	cp *csv ..
clean: force
	-rm *~ *.out
force:

