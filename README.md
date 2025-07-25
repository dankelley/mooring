
<!-- README.md is generated from README.Rmd, so please edit the latter. -->

# mooring

<!-- badges: start -->

[![R-CMD-check](https://github.com/dankelley/mooring/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dankelley/mooring/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The goal of the `mooring` package is to facilitate computations in the R
language of the deformation of oceanographic moorings by ocean currents.
It builds on Fortran code discussed by Moller (1976), which evolved into
a new form as discussed by Hamilton, Fowler, and Belliveau (1997) and
Hamilton (1989), and then a Matlab form discussed by Dewey (1999). It is
more limited than these earlier works, e.g. in considering only
unidirectional currents, in ignoring the stretching of mooring
components, and in ignoring the stresses involved during mooring
descent.

In addition to a fairly broad suite of functions, the package also
provides crude interactive tools for exploring simple moorings.

Conventional R help is provided, in addition to several vignettes, and a
[youtube
playlist](https://www.youtube.com/playlist?list=PLgOXS4jiqJftEEw-jS_V4edXWpaojcw-J&si=xkM2cH3u_e1gdsWU).

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
mooring shape, (b) set the `which` argument of `plot.mooring()` to focus
more directly on the knockdown, and (c) explore the effects of
specifying depth-dependent currents in the `knockdown()` call. After a
few steps like this, you ought to examine the vignettes and the
documentation of the package’s functions, so you can tackle a practical
case of your own.

``` r
library(mooring)
#> Loading required package: S7
# Design a mooring with a 16-inch Viny float attached to a bottom
# anchor with 100m of wire cable.
m <- mooring(anchor(), wire(length = 100), float("16in Viny"), waterDepth = 120)
# Segmentize wire portion (to 1m spacing).
ms <- segmentize(m)
# Apply a depth-invariant 0.5 m/s current.
msk <- knockdown(ms, u = 0.5)
plot(msk, fancy = TRUE)
```

<img src="man/figures/README-example-1.png" width="100%" />

# Acknowledgments

The mooring-element properties that are stored in the `mooringElements`
database stem from three main sources. The first was Richard Dewey’s
Matlab code (see Dewey (2023) for an updated version). The second was in
spreadsheets and other documents provided by Jay Barthelotte and Matthew
Lawson of the Bedford Institute of Oceanography
([BIO](https://www.bio.gc.ca/index-en.php)). And the third was in
spreadsheets and other documents provided by Danielle Dempsey and Nicole
Torrie of the Centre for Marine Research ([CMAR](https://cmar.ca)).
These sources are designated `"Dewey"`, `"BIO"` and `"CMAR"`,
respectively, in `mooringElements`.

Conversations with Clark Richards and Chantelle Layton (both of
[BIO](https://www.bio.gc.ca/index-en.php)) were helpful in identifying
data sources and in clarifying the nature of mooring systems.

# References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-dewey_mooring_1999" class="csl-entry">

Dewey, Richard K. 1999. “Mooring Design & Dynamics—a Matlab® Package for
Designing and Analyzing Oceanographic Moorings.” *Marine Models* 1 (1):
103–57. <https://doi.org/10.1016/S1369-9350(00)00002-X>.

</div>

<div id="ref-dewey_mooring_2023" class="csl-entry">

———. 2023. “Mooring Design and Dynamics: Users Guide.”

</div>

<div id="ref-hamilton_validation_1989" class="csl-entry">

Hamilton, James M. 1989. “The Validation and Practical Applications of a
Sub-Surface Mooring Model.” 119. Bedford Institute of Oceanography.
<https://waves-vagues.dfo-mpo.gc.ca/Library/112322.pdf>.

</div>

<div id="ref-hamilton_mooring_1997" class="csl-entry">

Hamilton, James M., George A Fowler, and Donald J. Belliveau. 1997.
“Mooring Vibration as a Source of Current Meter Error and Its
Correction.” *Journal of Atmospheric and Oceanic Technology* 14 (3):
644–55.
[https://doi.org/10.1175/1520-0426(1997)014\<0644:MVAASO\>2.0.CO;2](https://doi.org/10.1175/1520-0426(1997)014<0644:MVAASO>2.0.CO;2).

</div>

<div id="ref-moller_computer_1976" class="csl-entry">

Moller, Donald A. 1976. “A Computer Program for the Design and Static
Analysis of Single-Point Subsurface Mooring Systems: NOYFB.” WHOI-76-59.
Woods Hole, MA: Woods Hole Oceanographic Institution.

</div>

</div>
