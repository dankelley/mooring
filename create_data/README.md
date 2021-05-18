# Using the files in this directory.

* `make csv` reads the `mdcodes.mat` file from Dewey's Matlab package [Dewey
  1999; 2021], creating `cables_dewey.csv`, `floats_dewey.csv` and
`wires_dewey.csv`.

* `make rda` takes the `*_dewey.csv` files and creates `element_properties.rda`
  from them.  (TODO: make it merge in `cabless_new.csv` etc files. These are
what users can add to, with pull requests.)

* `make install` copies `element_properties.rda` to `../data`, where it can be
  accessed by the package.


# References

* Dewey, Richard K. “Mooring Design & Dynamics—a Matlab® Package for Designing
  and Analyzing Oceanographic Moorings.” Marine Models 1, no. 1 (December 1,
1999): 103–57. https://doi.org/10.1016/S1369-9350(00)00002-X.

* Dewey, Richard. “Mooring Design and Dynamics.” Accessed May 15, 2021.
  http://canuck.seos.uvic.ca/rkd/mooring/moordyn.php.

