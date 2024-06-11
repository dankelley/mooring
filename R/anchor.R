# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Create an anchor object
#'
#' Create a anchor object, either by looking up a known object from
#' the database, or by defining a new type. This must be the first
#' element of a mooring constructed with [mooring()].
#'
#' @templateVar subclass anchor
#'
#' @template modelTemplate
#'
#' @template buoyancyTemplate
#'
#' @template heightTemplate
#'
#' @template CDTemplate
#'
#' @template heightTemplate
#'
#' @return `anchor` returns a `"mooringElement"` object with `"anchor"` subclass.
#'
#' @family functions that create mooring elements
#'
#' @examples
#' library(mooring)
#' # List known anchor types
#' anchor("?")
#'
#' @export
#'
#' @author Dan Kelley
anchor <- function(model = "1 Railway Wheel", buoyancy = NULL, height = NULL, CD = NULL) {
    # message("about to try anchorS7...")
    # print(anchorS7())
    # message("... did it work?")
    data("mooringElements", package = "mooring", envir = environment())
    mooringElements <- get("mooringElements")
    if (model == "?") {
        return(sort(mooringElements$anchors$name))
    } else if (substring(model, 1, 1) == "?") {
        return(findElement(substring(model, 2), search = "anchor"))
    }
    w <- which(mooringElements$anchors$name == model)
    if (1 == length(w)) {
        me <- mooringElements$anchors[w, ]
        if (!is.null(buoyancy)) {
            warning("ignoring supplied buoyancy, because \"", model, "\" is already in the database\n")
        }
        if (!is.null(height)) {
            warning("ignoring supplied height, because \"", model, "\" is already in the database\n")
        }
        buoyancy <- me$buoyancy
        height <- me$height
        CD <- me$CD
        source <- me$source
        originalName <- me$originalName
    } else {
        if (is.null(buoyancy)) stop("must supply buoyancy, if creating a new anchor model")
        if (is.null(height)) stop("must supply height, if creating a new anchor model")
        if (is.null(CD)) stop("must supply CD, if creating a new anchor model")
        source <- ""
        originalName <- ""
    }
    anchorS7(model = model, buoyancy = buoyancy, height = height, area = 0, CD = CD, source = source, originalName = originalName)
} # anchor()

#' Find Anchor Weight of a Mooring
#'
#' @template mTemplate
#'
#' @return `anchorWeight` returns the weight of the mooring anchor,
#' in kg.
#'
#' @export
#'
#' @author Dan Kelley
anchorWeight <- function(m) {
    if (!is.mooring(m)) {
        stop("m must be a mooring object, created with mooring()")
    }
    # FIXME: check that it has more than 0 objects, and that one is an anchor
    look <- which(is.anchor(m))
    -m@elements[[look[1]]]@buoyancy # the [1] is just to be sure
}
