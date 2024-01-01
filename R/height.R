# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Get mooring/element height
#'
#' @template meTemplate
#'
#' @return `height` returns a numeric vector of element(s) height, in m.  This will be
#' a single value if the first element is a single element, or a longer vector if it
#' is a mooring.
#'
#' @examples
#' library(mooring)
#' height(float())
#' m <- mooring(anchor(depth = 120), wire(length = 100), float("HMB 20"))
#' height(m)
#'
#' @export
#'
#' @author Dan Kelley
height <- function(m) {
    if (is.mooring(m)) {
        sapply(m@elements, \(e) e@height)
    } else if (is.mooringElement(m)) {
        m@height
        #if (length(class(m)) == 2) m$height else stop("height can only be computed for a mooring or an element")
    }
}
