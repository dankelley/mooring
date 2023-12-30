# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Create an instrument object
#'
#' Create an instrument object,
#' either by looking up a known object from the database, or by defining a new type.
#'
#' @templateVar subclass instrument
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
#' @return `instrument` returns a `"mooringElement"` object with `"instrument"` subclass.
#'
#' @family functions that create mooring elements
#'
#' @examples
#' library(mooring)
#' # List known instrument types
#' instrument("?")
#'
#' @export
#'
#' @author Dan Kelley
instrument <- function(model = "SBE37 microcat clamp-on style", buoyancy = NULL, height = NULL, area = NULL, CD = NULL) {
    data("mooringElements", package = "mooring", envir = environment())
    mooringElements <- get("mooringElements")
    if (model == "?") {
        return(sort(mooringElements$instruments$name))
    } else if (substring(model, 1, 1) == "?") {
        return(findElement(substring(model, 2), search = "instrument"))
    }
    w <- which(mooringElements$instruments$name == model)
    if (1 == length(w)) {
        me <- mooringElements$instruments[w, ]
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
    } else {
        if (is.null(buoyancy)) stop("must supply buoyancy, if creating a new instrument model")
        if (is.null(height)) stop("must supply height, if creating a new instrument model")
        if (is.null(area)) stop("must supply area, if creating a new instrument model")
        if (is.null(CD)) stop("must supply CD, if creating a new instrument model")
        source <- ""
    }
    rval <- list(model = model, buoyancy = buoyancy, height = height, area = area, CD = CD, source = source)
    class(rval) <- c("mooringElement", "instrument")
    rval
} # instrument()
