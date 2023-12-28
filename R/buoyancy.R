# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Buoyancy of mooring elements
#'
#' The nonphysical unit of kg reflects a common convention used
#' by manufacturers of oceanographic mooring equipment. For calculations
#' of buoyancy *force*, the return value from this function
#' must be multiplied by the acceleration due to gravity,
#' g=9.8m/s^2.
#'
#' Note that the present version of this function
#' does not account for depth variations in seawater density,
#' for those tend to be well under 1 percent, and other
#' forces involved in mooring dynamics are much more uncertain
#' than that.  For example, Hamilton (1989) found that oscillations
#' in mooring lines could lead to enhanced drag, in some cases
#' necessitating an increase in CD for wire from 1.4 to 2.6
#' (see captions of his figures 12 and 13).
#'
#' @template meTemplate
#'
#' @template debugTemplate
#'
#' @return `buoyancy` returns a numeric vector of buoyancy, expressed in kg.
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
        mooringDebug(debug, "buoyancy(", length(m), "-element mooring):\n", sep = "")
        rval <- sapply(m, function(mi) buoyancy(mi, debug = debug))
    } else if (is.mooringElement(m)) {
        mooringDebug(debug, "    buoyancy(", class(m)[2], "): ", sep = "")
        rval <- if ("buoyancy" %in% names(m)) m$buoyancy else stop("no buoyancy in m")
    } else {
        rval <- NA
    }
    mooringDebug(debug, paste(rval, collapse = " "), " kg\n")
    rval
}
