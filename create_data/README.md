# Using the files in this directory.

Go into each subdirectory and use its Makefile to update things as follows:
* In `dewey` (which handles data from Dewey's matlab package; see references),
  do `make csv` and then `make install`.  The first of these operations the
`mdcodes.mat` file from Dewey's Matlab package [Dewey 1999; 2021], creating
`cables_dewey.csv`, `floats_dewey.csv` and `wires_dewey.csv`.  The second
operation copies them here.
* In `bio`, do `make install` since the csv files are hand-crafted there, so
  the only required action is to copy them here.

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

