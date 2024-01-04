# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4


#' Create a float object
#' @templateVar subclass float
#'
#' Create a float object, either by looking up a known object from the database,
#' or by defining a new type.
#' Area is formulated as pi*(diameter/2)^2, for consistency with Dewey's Matlab code.
#'
#' Note that `HMB` in a name is a short-hand for `Hydrofloat Mooring Buoy`.  Data for these
#' floats was extracted from Reference 1 on 2021-05-19 by Dan Kelley, with the `CD` value
#' being set at 0.65 (in the absence of any data from the manufacture) because that
#' value is used in Dewey's dataset for many floats.
#'
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
#' @return `float` returns a `"mooringElement"` object with `"float"` subclass.
#'
#' @references
#' 1. \url{https://deepwaterbuoyancy.com/wp-content/uploads/hydro-float-mooring-buoys-deepwater-buoyancy.pdf}
#'
#' @family functions that create mooring elements
#'
#' @examples
#' library(mooring)
#' # List known float types
#' float("?")
#'
#' @export
#'
#' @author Dan Kelley
float_test <- function(model = "Kiel SFS40in", buoyancy = NULL, height = NULL, area = NULL, CD = NULL) {
    data("mooringElements", package = "mooring", envir = environment())
    mooringElements <- get("mooringElements")
    if (model == "?") {
        return(sort(mooringElements$floats$name))
    } else if (substring(model, 1, 1) == "?") {
        return(findElement(substring(model, 2), search = "float"))
    }
    w <- which(mooringElements$floats$name == model)
    if (1 == length(w)) {
        me <- mooringElements$floats[w, ]
        if (!is.null(buoyancy)) {
            warning("ignoring supplied buoyancy, because \"", model, "\" is already in the database\n")
        }
        if (!is.null(height)) {
            warning("ignoring supplied height, because \"", model, "\" is already in the database\n")
        }
        if (!is.null(area)) {
            warning("ignoring supplied area, because \"", model, "\" is already in the database\n")
        }
        if (!is.null(CD)) {
            warning("ignoring supplied CD, because \"", model, "\" is already in the database\n")
        }
        buoyancy <- me$buoyancy
        height <- me$height
        area <- me$area
        CD <- me$CD
        source <- me$source
        originalName <- me$originalName
    } else {
        if (is.null(buoyancy)) stop("must supply buoyancy, if creating a new float model")
        if (is.null(height)) stop("must supply height, if creating a new float model")
        if (is.null(area)) stop("must supply area, if creating a new float model")
        if (is.null(CD)) stop("must supply CD, if creating a new float model")
        source <- ""
        originalName <- model
    }
    # Floats are assumed to be circular in the flow direction, following
    # Dewey's convention, so the area is pi*radius^2.
    float(model = model, buoyancy = buoyancy, height = height, area = area, CD = CD, source = source, originalName = model)
    # rval@model <- model
    # rval@buoyancy <- buoyancy
    # rval@height <- height
    # rval@area <- area
    # rval@CD <- CD
    # rval@source <- source
    # rval@originalName <- originalName
    # rval
} # float()
