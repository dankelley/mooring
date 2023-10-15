# Using the files in this directory.

Go into each subdirectory and use its Makefile to update things as follows.

First, visit the `dewey` directory, which handles data from Dewey's matlab
package; see references) In a unix terminal, type `make csv` to create the
files `cables_dewey.csv`, `floats_dewey.csv` and `wires_dewey.csv`, based on
the file `mdcodes.mat` that comes from Dewey's Matlab package [Dewey 1999;
2021]. You may wish to examine the csv files.  Once satisfied, type `make
install` to move the files.

Second, visit the `bio` directory, and use `make install` to install some csv
files.  (There is no need for a `make csv` step because BIO has provided .csv
files already.)

Once these things are done (and similar for other subdirectories, if they are
added), return here and do `make rda` to create a local rda file.  Examine it
as a cautionary measure, and then do `make install` to copy it into `../data`.
Then rebuild the package.

# References

* Dewey, Richard K. “Mooring Design & Dynamics—a Matlab® Package for Designing
  and Analyzing Oceanographic Moorings.” Marine Models 1, no. 1 (December 1,
  1999): 103–57. https://doi.org/10.1016/S1369-9350(00)00002-X.

* Dewey, Richard. “Mooring Design and Dynamics.” Accessed May 15, 2021.
  http://canuck.seos.uvic.ca/rkd/mooring/moordyn.php.

