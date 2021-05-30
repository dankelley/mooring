all:
	echo "author: do 'make csv' and then 'make install'; others: leave"
csv: force
	Rscript create_dewey_csv.R
install: force
	cp *csv ..
clean: force
	-rm *~ *.out
force:

