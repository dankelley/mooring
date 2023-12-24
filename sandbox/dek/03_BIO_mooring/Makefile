all: 03_BIO_mooring_01.out
%.out: %.R
	R --no-save < $< > $@
clean: force
	-rm -rf *.out *.png TAGS *~
force:

