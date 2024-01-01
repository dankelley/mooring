#' Create a chain object
#'
#' Create an object that describes mooring chain elements such as shackles,
#' either by looking up a known object from the database, or by defining a new type.
#' Area is formulated as length*width, for consistency with Dewey's Matlab code.
#'
#' @templateVar subclass chain
#' @template modelTemplate
#'
#' @template buoyancyPerMeterTemplate
#'
#' @template areaPerMeterTemplate
#'
#' @template CDTemplate
#'
#' @template sourceTemplate
#'
#' @template originalNameTemplate
#'
#' @param length (mandatory) numeric value indicating the length (in m) of the wire.
#'
#' @template sourceTemplate
#'
#' @return `chain` returns an object of the `"mooringElement"` class and `"chain"` subclass.
#'
#' @family functions that create mooring elements
#'
#' @export
#'
#' @importFrom utils data
#'
#' @examples
#' library(mooring)
#' # List known chain types
#' chain("?")
#'
#' @author Dan Kelley
chain <- function(model = "1in buoy chain", buoyancyPerMeter = NULL, areaPerMeter = NULL, CD = NULL, length = NULL, source = NULL, originalName = NULL) {
    data("mooringElements", package = "mooring", envir = environment())
    mooringElements <- get("mooringElements")
    if (model == "?") {
        return(sort(mooringElements$chains$name))
    } else if (substring(model, 1, 1) == "?") {
        return(findElement(substring(model, 2), search = "chain"))
    }
    if (is.null(length)) {
        stop("must supply length")
    }
    w <- which(mooringElements$chains$name == model)
    if (1 == length(w)) {
        me <- mooringElements$chains[w, ]
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
        originalName <- me$originalName
    } else {
        if (is.null(buoyancyPerMeter)) stop("must supply buoyancyPerMeter, if creating a new chain model")
        if (is.null(areaPerMeter)) stop("must supply areaPerMeter, if creating a new chain model")
        if (is.null(CD)) stop("must supply CD, if creating a new chain model")
        source <- ""
        originalName <- ""
    }
    chainS7(model = model, buoyancy = length * buoyancyPerMeter, height = length, area = length * areaPerMeter, CD = CD, source = source, originalName = originalName)
} # chain()
