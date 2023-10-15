# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Depth of mooring elements
#'
#' This is the depth of the *top* of the element. See also
#' [z()], which is the negative of the result from `depth()`.
#'
#' @template mTemplate
#'
#' @template stagnantTemplate
#'
#' @template skipWire
#'
#' @return a numeric vector of depth in metres.
#'
#' @examples
#' library(mooring)
#' m <- mooring(anchor(depth=120), wire(length=100), float("HMB 20"))
#' depth(m)
#'
#' @export
#'
#' @author Dan Kelley
depth <- function(m, stagnant=FALSE, skipWire=FALSE)
{
    -z(m, stagnant=stagnant, skipWire=skipWire)
}
