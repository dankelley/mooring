# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Horizontally-projected area of mooring elements
#'
#' The areas are not computed by this function, but rather looked up
#' for each element.  To learn how areas are computed during
#' setup, see the help page for
#' [anchor()],
#' [chain()],
#' [connector()],
#' [float()],
#' [instrument()],
#' [misc()], or
#' [release()].
#'
#' For a summary of characteristics of the predefined models,
#' see the vignette named "Default Values for Mooring Elements".
#'
#' @template meTemplate
#'
#' @param phi angle (in degrees) of element compared with a vertical line. For
#' is used for wire, chain, and intrument elements, but not for floats, which
#' are assumed to be spherical.
#'
#' @return `area` returns a numeric value of the side-view area, in m^2.
#'
#' @examples
#' library(mooring)
#' area(float())
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
area <- function(m, phi = 0.0) {
    if (!inherits(m, "mooring")) {
        stop("only works for objects created by mooring(), or by float(), etc")
    }
    areaFactor <- ifelse(is.float(m), 1.0, cos(phi * pi / 180))
    if (is.mooring(m)) {
        #message("area case 1")
        #print(areaFactor)
        #print(sapply(m, \(mi) mi$area))
        areaFactor * sapply(m, \(mi) mi$area)
    } else {
        #message("area case 1")
        if (length(class(m)) != 1L) m$area else stop("area can only be computed for a mooring or an individual element")
    }
}
