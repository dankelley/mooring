# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Get Mooring/Element side-view Area
#'
#' `area` returns a numeric vector of mooring element side-view
#' area(s), in square metres, taking into account the orientation of
#' the element with respect to the vertical. (Floats are assumed to be
#' spherical, so the orientation is irrelevant. This is not the case
#' for wires and instruments.) The return value will be a single
#' number if `m` is a single element but in a typical mooring it will
#' have multiple values. The areas are listed with the top element
#' first, i.e. in the reverse order to that used in constructing the
#' mooring with [mooring()].
#'
#' When initially created, objects do not contain an angle value, but
#' such values are inserted by [knockdown()]. Moorings that are
#' significantly tilted will have reduced side-view area, which
#' will reduce the current-induced drag, as computed with [drag()].
#'
#' @template meTemplate
#'
#' @param phi a numeric or logical value that controls how the angle
#' is computed.  If this is FALSE, no angle is used.  If it is TRUE,
#' which is the default, then the value of `phi` within the mooring
#' element is used (or 0 is used, if there is no value).  Finally, if
#' `phi` is a single numerical value, then that is taken as the angle
#' to the vertical, in radians.
#'
#' @return `area` returns a numeric value of the side-view area, in m^2.
#'
#' @examples
#' library(mooring)
#' # Floats are unaffected by rotation
#' area(float())
#' area(float(), pi / 4)
#' # Other elements, e.g. instruments, are affected by rotation
#' area(instrument())
#' area(instrument(), pi / 4.0) * sqrt(2.0)
#'
## Remove these references, which belong only at the top level, where users
## will expect to see them, and where they can be edited in a single spot.
## @references
## Dewey, Richard K. "Mooring Design & Dynamics-a Matlab® Package for
## Designing and Analyzing Oceanographic Moorings." Marine Models, vol. 1, no. 1
## (December 1, 1999): 103–57. https://doi.org/10.1016/S1369-9350(00)00002-X
##
## Dewey, Richard. "Mooring Design and Dynamics:
## A Matlab Package for Designing and Testing
## Oceanographic Moorings And Towed Bodies."
## Accessed May 15, 2021.
## http://canuck.seos.uvic.ca/rkd/mooring/mdd/mdd.php
## http://canuck.seos.uvic.ca/rkd/mooring/moordyn.php
#'
#' @export
#'
#' @author Dan Kelley
area <- function(m, phi = TRUE) {
    if (is.mooring(m)) {
        sapply(m@elements, \(e) area(e, phi))
    } else if (is.mooringElement(m)) {
        if (is.float(m)) {
            areaFactor <- 1.0
        } else {
            if (is.logical(phi)) {
                if (phi) {
                    areaFactor <- if (length(m@phi)) cos(m@phi) else 1 # FIXME: is this a good assumption?
                } else {
                    areaFactor <- 1.0
                }
            } else {
                areaFactor <- cos(phi)
            }
        }
        areaFactor * m@area
    } else {
        stop("area can only be computed for a mooring or an individual element")
    }
}
