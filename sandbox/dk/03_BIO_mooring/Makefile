all: 03_BIO_mooring_01.out
%.out: %.R
	Rscript $< > $@
clean: force
	-rm -rf *.out *.png TAGS *~
force:

