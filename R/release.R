#' Create a mooring-release object
#'
#' Create a mooring-release object,
#' either by looking up a known object from the database, or by defining a new type.
#' Area is formulated as length*width, for consistency with Dewey's Matlab code.
#'
#' @templateVar subclass release
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
#' @return `release` returns an object of the `"mooringElement"` class and `"release"` subclass.
#'
#' @examples
#' library(mooring)
#' # List known wire types
#' release("?")
#'
#' @family functions that create mooring elements
#'
#' @export
#'
#' @author Dan Kelley
release <- function(model = "EG&G 723a", buoyancy = NULL, height = NULL, area = NULL, CD = NULL) {
    data("mooringElements", package = "mooring", envir = environment())
    mooringElements <- get("mooringElements")
    if (model == "?") {
        return(sort(mooringElements$releases$name))
    } else if (substring(model, 1, 1) == "?") {
        return(findElement(substring(model, 2), search = "release"))
    }
    w <- which(mooringElements$releases$name == model)
    if (1 == length(w)) {
        me <- mooringElements$releases[w, ]
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
        if (is.null(buoyancy)) stop("must supply buoyancy, if creating a new release model")
        if (is.null(height)) stop("must supply height, if creating a new release model")
        if (is.null(area)) stop("must supply area, if creating a new release model")
        if (is.null(CD)) stop("must supply CD, if creating a new release model")
        source <- ""
    }
    rval <- list(model = model, source = source, buoyancy = buoyancy, height = height, area = area, CD = CD)
    class(rval) <- c("mooringElement", "release")
    rval
} # release()
