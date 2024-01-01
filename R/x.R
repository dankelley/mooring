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
    message("in x")
    e <- m@elements
    rval <- if (stagnant) {
        if (length(e[[1]]@x0)) {
            sapply(e, \(ee) ee@x0)
        } else {
            sapply(e, \(ee) ee@x)
        }
    } else {
        sapply(e, \(ee) ee@x)
    }
    if (skipWire) rval[!is.wire(m)] else rval # FIXME: this is likely wrong
}
