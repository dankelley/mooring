
<!-- README.md is generated from README.Rmd, so please edit the latter. -->

# mooring

<!-- badges: start -->

[![R-CMD-check](https://github.com/dankelley/mooring/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dankelley/mooring/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The goal of the `mooring` package is to facilitate computations of the
extent of ‘knockdown’ of oceanographic moorings caused by ocean currents
that exert drag on mooring elements.

In addition to a fairly broad suite of functions, the package also
provides crude interactive tools for exploring simple moorings.

Conventional R help is provided, in addition to several vignettes.

At the moment, `mooring` only handles bottom-anchored moorings in which
the top buoyancy element is below the sub-surface.

## Installation

You can install the development version of `mooring` from
[GitHub](https://github.com/) by typing the following in an R console.
(Uncomment the first line, if the `devtools` package is not already
installed on your computer.)

``` r
# install.packages("devtools")
devtools::install_github("dankelley/mooring")
```

## Example

The following shows how to assess the knockdown of a mooring consisting
of a bottom anchor, 100 m of wire, and a 16-inch Viny float, in a region
of water depth 120 m with a constant current of 0.5 m/s (roughly 1
knot). To learn more, (a) alter the current and observe changes in the
mooring shape, (b) set the `which` argument of `plot.mooring()`, to
focus more directly on the knockdown, then (c) explore the effects of
specifying depth-dependent currents in the `knockdown()` call.

``` r
library(mooring)
#> Loading required package: S7
# Design a mooring with a 16-inch Viny float attached to a bottom
# anchor with 100m of wire cable.
m <- mooring(anchor(), wire(length = 100), float("16in Viny"), waterDepth = 120)
# Discretise wire portion (to 1m spacing).
md <- discretise(m)
# Apply a 1 m/s current.
mdk <- knockdown(md, u = 0.5)
draw(mdk, fancy = TRUE)
```

<img src="man/figures/README-example-1.png" width="100%" />

# Acknowledgements

The mooring-element properties that are stored in the `mooringElements`
database stem from three main sources. The first was Richard Dewey’s
excellent mooring-analysis Matlab software, described by Dewey (1999).
The second was in spreadsheets and other documents provided by Jay
Barthelotte and Matthew Lawson of the Bedford Institute of Oceanography
([BIO](https://www.bio.gc.ca/index-en.php)). And the third was in
spreadsheets and other documents provided by Danielle Dempsey and Nicole
Torrie of the Centre for Marine Research ([CMAR](https://cmar.ca)).
These sources are designated `"Dewey"`, `"BIO"` and `"CMAR"`,
respectively, in `mooringElements`.

Conversations with Clark Richards and Chantelle Layton (both of
[BIO](https://www.bio.gc.ca/index-en.php)) were helpful in identifying
data sources and in clarifying the nature of mooring systems. Documents
by J. M. Hamilton (1989) and James M. Hamilton, Fowler, and Belliveau
(1997) cast further light on important practicalities.

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-dewey_mooring_1999" class="csl-entry">

Dewey, Richard K. 1999. “Mooring Design & Dynamics—a Matlab® Package for
Designing and Analyzing Oceanographic Moorings.” *Marine Models* 1 (1):
103–57. <https://doi.org/10.1016/S1369-9350(00)00002-X>.

</div>

<div id="ref-hamilton_validation_1989" class="csl-entry">

Hamilton, J. M. 1989. “The Validation and Practical Applications of a
Sub-Surface Mooring Model.” 119. Bedford Institute of Oceanography.
<https://waves-vagues.dfo-mpo.gc.ca/Library/112322.pdf>.

</div>

<div id="ref-hamilton_mooring_1997" class="csl-entry">

Hamilton, James M., George A. Fowler, and Donald J. Belliveau. 1997.
“Mooring Vibration as a Source of Current Meter Error and Its
Correction.” *Journal of Atmospheric and Oceanic Technology* 14 (3):
644–55.

</div>

</div>
