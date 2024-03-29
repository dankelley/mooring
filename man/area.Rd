% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/area.R
\name{area}
\alias{area}
\title{Side-view Area of Mooring Element}
\usage{
area(m, phi = TRUE)
}
\arguments{
\item{m}{either a mooring object, created by \code{\link[=mooring]{mooring()}}, or a
mooring-element object, created with}

\item{phi}{a numeric or logical value that controls how the angle
is computed.  If this is FALSE, no angle is used.  If it is TRUE,
which is the default, then the value of \code{phi} within the mooring
element is used (or 0 is used, if there is no value).  Finally, if
\code{phi} is a single numerical value, then that is taken as the angle
to the vertical, in radians.}
}
\value{
\code{area} returns a numeric value of the side-view area, in m^2.
}
\description{
Returns the area of the mooring element, projected onto a vertical plane.
This is computed using the \code{area} item stored within the element,
adjusted for the angle, depending on the value of \code{phi}. Note that
elements created by \code{\link[=float]{float()}} are assumed to be spherical, and so
their orientation is not taken into account.
}
\details{
When initially created, objects do not contain an angle value, but
such values are inserted by \code{\link[=knockdown]{knockdown()}}. Moorings that are
significantly tilted will have reduced side-view area, which
will reduce the current-induced drag, as computed with \code{\link[=drag]{drag()}}.
}
\examples{
library(mooring)
# Floats are unaffected by rotation
area(float())
area(float(), pi / 4)
# Other elements, e.g. instruments, are affected by rotation
area(instrument())
area(instrument(), pi / 4.0) * sqrt(2.0)


}
\author{
Dan Kelley
}
