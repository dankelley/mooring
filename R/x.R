# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Horizontal coordinate of mooring elements
#'
#' @template mTemplate
#'
#' @template stagnantTemplate
#'
#' @template skipWire
#'
#' @return a numeric vector of horizontal coordinate in metres.
#'
#' @examples
#' library(mooring)
#' m <- mooring(anchor(depth = 120), wire(length = 100), float("HMB 20"))
#' x(m)
#'
#' @export
#'
#' @author Dan Kelley
x <- function(m, stagnant = FALSE, skipWire = FALSE) {
    rval <- if (stagnant) {
        if ("x0" %in% names(m[[1]])) {
            sapply(m, function(mi) mi$x0)
        } else {
            sapply(m, function(mi) mi$x)
        }
    } else {
        sapply(m, function(mi) mi$x)
    }
    if (skipWire) rval[!is.wire(m)] else rval
}
