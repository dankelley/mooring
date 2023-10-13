# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Create a misc object
#'
#' Create a miscellaneous object,
#' either by looking up a known object from the database, or by defining a new type.
#' The function name, and most of the built-in data
#' values, come from Dewey's (1999, 2021) database.
#' For data derived from the Dewey database,
#' area is computed as in Dewey's Matlab code: if tabulated diameter is zero,
#' then the product of tabulated height and width is used; otherwise,
#' pi*(diameter/2)^2 is used.
#'
#' @templateVar subclass misc
#' @template modelTemplate
#'
#' @template buoyancyTemplate
#'
#' @template heightTemplate
#'
#' @template areaTemplate
#'
#' @template CDTemplate
#'
#' @template sourceTemplate
#'
#' @return `misc` returns an object of the `"mooring"` class and `"misc"` subclass.
#'
#' @family functions that create mooring objects
#'
#' @examples
#' library(mooring)
#' # List known misc types
#' misc("?")
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
misc <- function(model="AanderaaT.chain", buoyancy=NULL, height=NULL, area=NULL, CD=NULL)
{
    data("mooringElements", package="mooring", envir=environment())
    mooringElements <- get("mooringElements")
    if (model == "?")
        return(sort(mooringElements$misc$name))
    else if (substring(model, 1, 1) == "?")
        return(findElement(substring(model, 2), search="instrument"))
    w <- which(mooringElements$misc$name == model)
    if (1 == length(w)) {
        me <- mooringElements$misc[w, ]
        if (!is.null(buoyancy))
            warning("ignoring supplied buoyancy, because \"", model, "\" is already in the database\n")
        if (!is.null(height))
            warning("ignoring supplied height, because \"", model, "\" is already in the database\n")
        if (!is.null(area))
            warning("ignoring supplied area, because \"", model, "\" is already in the database\n")
        if (!is.null(CD))
            warning("ignoring supplied CD, because \"", model, "\" is already in the database\n")
        buoyancy <- me$buoyancy
        height <- me$height
        area <- me$area
        CD <- me$CD
        source <- me$source
    } else {
        if (is.null(buoyancy)) stop("must supply buoyancy, if creating a new misc model")
        if (is.null(height)) stop("must supply height, if creating a new misc model")
        if (is.null(area)) stop("must supply area, if creating a new misc model")
        if (is.null(CD)) stop("must supply CD, if creating a new misc model")
        source <- ""
    }
    rval <- list(model=model, buoyancy=buoyancy, height=height, area=area, CD=CD, source=source)
    class(rval) <- c("mooring", "misc")
    rval
}                                      # misc()
