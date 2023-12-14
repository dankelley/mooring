all:
	echo "Only the author should be working here"
local: force
	Rscript cmar.R > cmar.out
install: force
	cp *_cmar.csv ..
clean: force
	-rm *~ *.out
force:

