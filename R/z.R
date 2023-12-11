# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Vertical coordinate of mooring elements
#'
#' This is the z coordinate of the *top* of the element. See also
#' [depth()], which is the negative of the result from `z()`.
#'
#' @template mTemplate
#'
#' @template stagnantTemplate
#'
#' @template skipWire
#'
#' @return a numeric vector of vertical coordinate in metres.
#'
#' @examples
#' library(mooring)
#' m <- mooring(anchor(depth = 120), wire(length = 100), float("HMB 20"))
#' z(m)
#'
#' @export
#'
#' @author Dan Kelley
z <- function(m, stagnant = FALSE, skipWire = FALSE) {
    rval <- if (stagnant) {
        if ("z0" %in% names(m[[1]])) {
            sapply(m, function(mi) mi$z0)
        } else {
            sapply(m, function(mi) mi$z)
        }
    } else {
        sapply(m, function(mi) mi$z)
    }
    if (skipWire) rval[!isWire(m)] else rval
}
