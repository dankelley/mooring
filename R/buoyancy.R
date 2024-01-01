# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Buoyancy of mooring elements
#'
#' This returns 'buoyancy' in kg, as is the convention of storage in
#' this package (and is quite common in practical work).  It must be
#' multiplied by the acceleration due to gravity, g=9.81m/s^2, to get
#' an *actual* buoyancy force, in Newtons. This multiplication is done
#' in [knockdown()] and [tension()].
#'
#' Note that the present version of this function does not account for
#' depth variations in seawater density, for those tend to be well
#' under 1 percent.
## and other forces involved in mooring dynamics are
## much more uncertain than that.  For example, Hamilton (1989) found
## that oscillations in mooring lines could lead to enhanced drag, in
## some cases necessitating an increase in CD for wire from 1.4 to 2.6
## (see captions of his figures 12 and 13).
#'
#' @template meTemplate
#'
#' @template debugTemplate
#'
#' @return `buoyancy` returns a numeric vector of buoyancy, expressed
#' in kg or N, depending on the value of `unit`.
#'
#' @examples
#' library(mooring)
#' m <- mooring(anchor(depth = 120), wire(length = 100), float("HMB 20"))
#' buoyancy(m)
#'
#' @export
#'
## @references
## Hamilton, J. M. "The Validation and Practical Applications of a Sub-Surface
## Mooring Model." Canadian Technical Report of Hydrography and Ocean
## Sciences. Bedford Institute of Oceanography, 1989.
#'
#' @author Dan Kelley
buoyancy <- function(m, debug = 0L) {
    if (is.mooring(m)) {
        mooringDebug(debug, "computing buoyancy for ", length(m@elements), "-element mooring\n", sep = "")
        rval <- sapply(m@elements, function(mi) buoyancy(mi, debug = debug))
    } else if (is.mooringElement(m)) {
        mooringDebug(debug, "computing buoyancy for a ", class(m)[1], " element\n", sep = "")
        rval <- m@buoyancy # FIXME: what about chopped-up wire/chain?
    } else {
        warning("object is neither a mooring nor a mooringElement")
        rval <- NA
    }
    mooringDebug(debug, paste(rval, collapse = " "), " kg\n")
    rval
}
