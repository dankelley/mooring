% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mooring.R
\name{isMooring}
\alias{isMooring}
\title{Detect whether an object is a mooring}
\usage{
isMooring(m = NULL)
}
\arguments{
\item{m}{an object to be tested}
}
\description{
An object is a mooring if it is a list that has more than one element, and
if each element inherits from the \code{"mooring"} class.  For example, the
output of \code{\link[=mooring]{mooring()}} is a mooring, but the output of \code{\link[=anchor]{anchor()}} is not.
This function is mainly designed for use within the package that that, e.g.
\code{\link[=knockdown]{knockdown()}} will produce an error if its first argument is not a mooring.
}
