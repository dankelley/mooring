% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mooring.R
\name{anchor}
\alias{anchor}
\title{Create an anchor object}
\usage{
anchor(
  model = "3 trainwheels",
  buoyancy = NULL,
  height = NULL,
  CD = NULL,
  depth = 0
)
}
\arguments{
\item{model}{character value indicating the type of anchor.
There are three possibilities for \code{model}.
(1) If this is \code{"?"}, then the function returns a vector of permitted
character values. (2) If this is \code{"?X"}, where X is a set of
characters, then \code{\link[=findElement]{findElement()}} is called to do a fuzzy search,
with the \code{search} argument set to \code{"anchor"}.
(3) If it is recognized, i.e. if that type is
stored in \code{data(mooringElements)}, then the that stored value
is used, and all other arguments to this function are ignored.
(4) Otherwise, a new anchor object is created, using
values specified in the other arguments (all of which must
be supplied).}

\item{buoyancy}{numeric value indicating buoyancy.  For
wire elements, this is buoyancy per length, expressed in kg/m.
For other elements, it is buoyancy, expressed in kg.
Note that \code{buoyancy} is ignored if \code{model} is recognized.}

\item{height}{numeric value for the height (in m) of the element.
This value is used in the computation of the mooring geometry.
In some cases, as e.g. in Dewey-derived data that have "diameter" tabulated
as zero, it is also used in the computation of the area
of the element, as height*width.
Note that \code{height} is ignored if \code{model} is recognized.}

\item{CD}{numeric value (unitless) for the drag coefficient.
Note that \code{CD} is ignored if \code{model} is recognized.}

\item{depth}{numeric value giving water depth in m.}
}
\value{
\code{anchor} returns an object of the \code{"mooring"} class and \code{"anchor"} subclass.
}
\description{
Create a anchor object,
either by looking up a known object from the database, or by defining a new type.
This must be the first element of a mooring constructed with
\code{\link[=mooring]{mooring()}}.  The default is 3 trainwheels with zero height (to simplify
test cases).
Note that \code{depth} is not a characteristic of the anchor, but rather of
the domain into which it is placed.
}
\section{Data sources}{


To learn more about the data used for the built-in
anchor
objects, use e.g.
\preformatted{
data(mooringElements)
mooringElements$anchors
}
}

\examples{
library(mooring)
# List known anchor types
anchor("?")

}
\seealso{
Other functions that create mooring objects: 
\code{\link{chain}()},
\code{\link{connector}()},
\code{\link{float}()},
\code{\link{instrument}()},
\code{\link{misc}()},
\code{\link{release}()},
\code{\link{wire}()}
}
\author{
Dan Kelley
}
\concept{functions that create mooring objects}
