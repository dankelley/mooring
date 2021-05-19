# Using the files in this directory.

* `make csv` reads the `mdcodes.mat` file from Dewey's Matlab package [Dewey
  1999; 2021], creating `cables_dewey.csv`, `floats_dewey.csv` and
`wires_dewey.csv`.

* `make rda` takes the `*_dewey.csv` files *plus* `chain.csv`, `float.csv`, and
  `wire.csv` (to which new items may be added), creating `mooringElements.rda`.

* `make install` copies `mooringElements.rda` to `../data`, so it can be
  accessed with `data(mooring_elements)`.

AUTHOR/CONTRIBUTOR NOTE: if new items are added to `chain.csv`, `float.csv` or
`wire.csv`, the author must rebuild, then use e.g. `float("?")` to get a list,
and then put that list into the docs for the function (look for the lines
containing the string `"FIXME: rebuild"`).

# References

* Dewey, Richard K. “Mooring Design & Dynamics—a Matlab® Package for Designing
  and Analyzing Oceanographic Moorings.” Marine Models 1, no. 1 (December 1,
1999): 103–57. https://doi.org/10.1016/S1369-9350(00)00002-X.

* Dewey, Richard. “Mooring Design and Dynamics.” Accessed May 15, 2021.
  http://canuck.seos.uvic.ca/rkd/mooring/moordyn.php.

