# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Get Mooring/Element Height
#'
#' @template meTemplate
#'
#' @return `height` returns a numeric vector of mooring element height(s), in
#' metres.  This will be a single value if `m` is a single element (as in
#' the first Example) or a vector, for a typical mooring, which has
#' multiple elements (as in the second Example). Note that, in the
#' latter case, the heights are listed with the top element first,
#' i.e. in the reverse order to that used in constructing the mooring
#' with [mooring()].
#'
#' @examples
#' library(mooring)
#' # Example 1. height of an individual element
#' height(float("HMB 20"))
#'
#' # Example 2. height of individual mooring elements, starting
#' # with the float.
#' m <- mooring(anchor(), wire(length = 100), float("HMB 20"), waterDepth = 120)
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
    } else {
        stop("height can only be computed for a mooring or an individual element")
    }
}
