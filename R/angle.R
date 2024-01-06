# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Angle of mooring elements
#'
#' This is the angle (in radians) between the element and
#' the vertical.
#'
#' @template mTemplate
#'
#' @template stagnantTemplate
#'
#' @template skipWire
#'
#' @return a numeric vector of the angle in radians.
#'
#' @examples
#' library(mooring)
#' m <- mooring(anchor(), wire(length = 100), float("HMB 20"), waterDepth = 120) |> knockdown(u = 1)
#' angle(m)
#'
#' @export
#'
#' @author Dan Kelley
angle <- function(m, stagnant = FALSE, skipWire = FALSE) {
    e <- m@elements
    # message("in angle 1")
    rval <- if (stagnant) {
        # message("in angle 2 (stagnant)")
        rep(0, length(e))
    } else {
        # message("in angle 3 (not stagnant)")
        sapply(e, \(ee) ee@phi)
    }
    if (skipWire) rval[!is.wire(m)] else rval # FIXME: this is likely wrong
}
