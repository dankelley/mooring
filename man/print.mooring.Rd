% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mooring.R
\name{print.mooring}
\alias{print.mooring}
\title{Print a mooring}
\usage{
\method{print}{mooring}(x, ...)
}
\arguments{
\item{x}{an object of the \code{"mooring"} class.}

\item{...}{optional arguments.  If this includes \code{debug}, and if that holds
a number greater than zero, then some debugging information is printed.}
}
\description{
Print a mooring
}
\examples{
library(mooring)
m <- mooring(anchor(depth=100), wire(length=80), float("HMB 20"))

}
\author{
Dan Kelley
part of the item.
}
