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
#' see the vignette named
#'
## @template mooringElementVignetteName
#'
#' @template meTemplate
#'
#' @return `area` returns a numeric value of the area viewed from a horizontal, in m^2.
#'
#' @examples
#' library(mooring)
#' area(float())
#'
#' @references
#' Dewey, Richard K. "Mooring Design & Dynamics-a Matlab® Package for
#' Designing and Analyzing Oceanographic Moorings." Marine Models, vol. 1, no. 1
#' (December 1, 1999): 103–57. https://doi.org/10.1016/S1369-9350(00)00002-X
#'
#' Dewey, Richard. "Mooring Design and Dynamics:
#' A Matlab Package for Designing and Testing
#' Oceanographic Moorings And Towed Bodies."
#' Accessed May 15, 2021.
#' http://canuck.seos.uvic.ca/rkd/mooring/mdd/mdd.php
#' http://canuck.seos.uvic.ca/rkd/mooring/moordyn.php
#'
#' @export
#'
#' @author Dan Kelley
area <- function(m)
{
    if (!inherits(m, "mooring"))
        stop("only works for objects created by mooring(), or by float(), etc")
    if (isMooring(m)) {
        sapply(m, function(mi) mi$area)
    } else {
        if (length(class(m)) == 2) m$area else stop("area can only be computed for a mooring or an individual element")
    }
}
