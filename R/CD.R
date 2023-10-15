# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Get mooring/element drag coefficient
#'
#' @template meTemplate
#'
#' @return `CD` returns a numeric vector of drag coefficient(s).
#'
#' @examples
#' library(mooring)
#' CD(float())
#'
#' @export
#'
#' @author Dan Kelley
CD <- function(m)
{
    if (isMooring(m)) {
        sapply(m, function(item) item$CD)
    } else {
        if (length(class(m)) == 2) m$CD else stop("area can only be computed for a mooring or an element")
    }
}
