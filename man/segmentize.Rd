% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/segmentize.R
\name{segmentize}
\alias{segmentize}
\title{Decompose chain and wire portions into shorter segments}
\usage{
segmentize(m, by = 1, debug = 0)
}
\arguments{
\item{m}{a mooring object, created by \code{\link[=mooring]{mooring()}}.}

\item{by}{numeric value giving the rough size of the chunks,
which is 1m by default. This is considered in the context of
the total length of the element, \code{L} say. If \code{L/by} exceeds
20, then chunks of length \code{by} are used.  Otherwise,
20 chunks, each of length \code{L/20}, are used.}

\item{debug}{an integer value (or a numeric that can be converted to an
integer) that controls debugging. If this is positive, then some debugging
information may be printed by the function.}
}
\value{
\code{segmentize} returns a \code{"mooring"} object, based
on \code{m} except that wire portions are chopped up into shorter
pieces.
}
\description{
Break up \code{chain} and \code{wire} portions of a mooring into smaller
chunks, so that the deformation by a current can be traced more
accurately by \code{\link[=knockdown]{knockdown()}}.
}
\author{
Dan Kelley
}
