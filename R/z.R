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
#' m <- mooring(anchor(), wire(length = 100), float("HMB 20"), waterDepth = 120)
#' z(m)
#'
#' @export
#'
#' @author Dan Kelley
z <- function(m, stagnant = FALSE, skipWire = FALSE) {
    e <- m@elements
    # message("in z 1")
    rval <- if (stagnant) {
        # message("in z 2 (stagnant)")
        if (length(e[[1]]@z0)) {
            # message("in z 2 (stagnant) z0 is not empty")
            sapply(e, \(ee) ee@z0)
        } else {
            # message("in z 2 (stagnant) z0 is empty, so using z")
            sapply(e, \(ee) ee@z)
        }
    } else {
        # message("in z 3 (not stagnant)")
        sapply(e, \(ee) ee@z)
    }
    if (skipWire) rval[!is.wire(m)] else rval # FIXME: this is likely wrong
}
