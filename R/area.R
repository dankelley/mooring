# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Side-view Area of Mooring Element
#'
#' Returns the area of the mooring element, projected onto a vertical plane.
#' This is computed using the `area` item stored within the element,
#' adjusted for the angle, depending on the value of `phi`. Note that
#' elements created by [float()] are assumed to be spherical, and so
#' their orientiation is not taken into account.
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
#' then a value of `phi` is sought within `me`, and that is used,
#' or else 0 is used. Finally, if `phi` is numeric, then it is taken
#' to be the angle the element makes to the vertical, in radians.
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
        # message("area case 1")
        # print(areaFactor)
        # print(sapply(m, \(mi) mi$area))
        sapply(m, \(mi) area(mi, phi))
    } else if (is.mooringElement(m)) {
        if (is.float(m)) {
            #message("Dan 1")
            areaFactor <- 1.0
        } else {
            if (is.logical(phi)) {
                #message("Dan 2")
                if (phi) {
                    #message("Dan 2a")
                    areaFactor <- if (is.null(m$phi)) 1.0 else cos(m$phi)
                } else {
                    #message("Dan 2b")
                    areaFactor <- 1.0
                }
            } else {
                #message("Dan 3")
                areaFactor <- cos(phi)
            }
        }
        #message("areaFactor=", areaFactor)
        areaFactor * m$area
    } else {
        stop("area can only be computed for a mooring or an individual element")
    }
}
