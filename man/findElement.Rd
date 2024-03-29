% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_element.R
\name{findElement}
\alias{findElement}
\title{Fuzzy search for mooring elements}
\usage{
findElement(
  e,
  search = c("anchor", "chain", "connector", "float", "instrument", "wire"),
  ignore.case = TRUE,
  max.distance = 0.1
)
}
\arguments{
\item{e}{character value to be used for the fuzzy match, passed on to \code{\link[=agrep]{agrep()}}.}

\item{search}{character vector holding the categories to be searched for.}

\item{ignore.case}{logical value, passed to \code{\link[=agrep]{agrep()}}. The default is to ignore case.}

\item{max.distance}{numeric value, passed to \code{\link[=agrep]{agrep()}}. The default usually catches
relevant cases; see the documentation for \code{\link[=agrep]{agrep()}} for the (somewhat subtle) meaning
of this argument.}
}
\value{
\code{findElement} returns (silently) a list of the suggested function calls.
}
\description{
\code{findElement} does a fuzzy search for an element model, using
\code{\link[=agrep]{agrep()}}.  The output (if any) is in the form of suggested calls
to element-creating functions
\code{\link[=anchor]{anchor()}}, \code{\link[=chain]{chain()}}, \code{\link[=connector]{connector()}}, \code{\link[=float]{float()}}, \code{\link[=instrument]{instrument()}}, \code{\link[=misc]{misc()}}
and \code{\link[=wire]{wire()}}.
The list is in alphabetical order, not the order of the closeness of the match.
}
\details{
\code{findElement} is used by e.g. \code{float("?BUB")}.
}
\examples{
library(mooring)
findElement("nylon")

}
\author{
Dan Kelley
}
