all:
	echo "author: do 'make csv install'; others: please do not use 'make'"
csv: force
	Rscript create_dewey_csv.R > create_dewey_csv.out
install: force
	cp *csv ..
clean: force
	-rm *~ *.out
force:

