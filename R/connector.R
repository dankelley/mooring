# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Create a connector object
#'
#' Create an object that describes mooring connector such as shackles,
#' either by looking up a known object from the database, or by defining a new type.
#' Area is formulated as height*width, for consistency with Dewey's Matlab code.
#'
#' Also, it is worth noting that there are built-in connector objects that might not
#' be thought of as connectors, e.g. `"ballast weight"`.
#'
#' @templateVar subclass connector
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
#' @return `connector` returns a `"mooringElement"` object with `"connector"` subclass.
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
#' connector("?")
#'
#' @author Dan Kelley
connector <- function(model = "swivel", buoyancy = NULL, height = NULL, area = NULL, CD = NULL) {
    data("mooringElements", package = "mooring", envir = environment())
    mooringElements <- get("mooringElements")
    if (model == "?") {
        return(sort(mooringElements$connectors$name))
    } else if (substring(model, 1, 1) == "?") {
        return(findElement(substring(model, 2), search = "connector"))
    }
    w <- which(mooringElements$connectors$name == model)
    if (1 == length(w)) {
        me <- mooringElements$connectors[w, ]
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
        if (is.null(buoyancy)) stop("must supply buoyancy, if creating a new connectors model")
        if (is.null(height)) stop("must supply height, if creating a new connectors model")
        if (is.null(area)) stop("must supply area, if creating a new connectors model")
        if (is.null(CD)) stop("must supply CD, if creating a new connectors model")
        source <- ""
    }
    connectorS7(model = model, buoyancy = buoyancy, height = height, area = area, CD = CD, source = source, originalName = originalName)
} # connector()
