# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Create a wire object
#'
#' Creates a wire (or rope, chain, etc.) object,
#' either by looking up a known object from the database, or by defining a new type.
#'
#' @templateVar subclass wire
#' @template modelTemplate
#'
#' @template buoyancyPerMeterTemplate
#'
#' @template areaPerMeterTemplate
#'
#' @template CDTemplate
#'
#' @param length (mandatory) numeric value indicating the length (in m) of the wire.
#'
#' @template sourceTemplate
#'
#' @return `wire` returns a `"mooringElement"` object with `"wire"` subclass.
#'
#' @family functions that create mooring elements
#'
#' @examples
#' library(mooring)
#' # List known wire types
#' wire("?")
#'
#' @export
#'
#' @importFrom utils data
#'
#' @author Dan Kelley
wire <- function(model = "1/4in wire/jack", buoyancyPerMeter = NULL, areaPerMeter = NULL, CD = NULL, length = NULL) {
    data("mooringElements", package = "mooring", envir = environment())
    mooringElements <- get("mooringElements")
    if (model == "?") {
        return(sort(mooringElements$wires$name))
    } else if (substring(model, 1, 1) == "?") {
        return(findElement(substring(model, 2), search = "wire"))
    }
    if (is.null(length)) {
        stop("must supply length")
    }
    w <- which(mooringElements$wires$name == model)
    if (1 == length(w)) {
        me <- mooringElements$wires[w, ]
        if (!is.null(buoyancyPerMeter)) {
            warning("ignoring supplied buoyancyPerMeter, because \"", model, "\" is already in the database\n")
        }
        if (!is.null(areaPerMeter)) {
            warning("ignoring supplied areaPerMeter, because \"", model, "\" is already in the database\n")
        }
        if (!is.null(CD)) {
            warning("ignoring supplied CD, because \"", model, "\" is already in the database\n")
        }
        buoyancyPerMeter <- me$buoyancyPerMeter
        areaPerMeter <- me$areaPerMeter
        CD <- me$CD
        source <- me$source
    } else {
        if (is.null(buoyancyPerMeter)) stop("must supply buoyancyPerMeter, if creating a new wire model")
        if (is.null(areaPerMeter)) stop("must supply areaPerMeter, if creating a new wire model")
        if (is.null(CD)) stop("must supply CD, if creating a new wire model")
        source <- ""
    }
    rval <- list(model = model, buoyancy = length * buoyancyPerMeter, height = length, area = length * areaPerMeter, CD = CD, source = source)
    class(rval) <- c("mooringElement", "wire")
    rval
} # wire()
