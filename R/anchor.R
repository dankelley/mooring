# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Create an anchor object
#'
#' Create a anchor object,
#' either by looking up a known object from the database, or by defining a new type.
#' This must be the first element of a mooring constructed with
#' [mooring()].  The default is 3 trainwheels with zero height (to simplify
#' test cases).
#' Note that `depth` is not a characteristic of the anchor, but rather of
#' the domain into which it is placed.
#'
#' @templateVar subclass anchor
#' @template modelTemplate
#'
#' @template buoyancyTemplate
#'
#' @template heightTemplate
#'
#' @template CDTemplate
#'
#' @template heightTemplate
#' @param depth numeric value giving water depth in m.
#'
#' @template sourceTemplate
#'
#' @return `anchor` returns an object of the `"mooring"` class and `"anchor"` subclass.
#'
#' @family functions that create mooring objects
#'
#' @examples
#' library(mooring)
#' # List known anchor types
#' anchor("?")
#'
#' @export
#'
#' @author Dan Kelley
anchor <- function(model="3 trainwheels", buoyancy=NULL, height=NULL, CD=NULL, depth=0)
{
    data("mooringElements", package="mooring", envir=environment())
    mooringElements <- get("mooringElements")
    if (model == "?")
        return(sort(mooringElements$anchors$name))
    else if (substring(model, 1, 1) == "?")
        return(findElement(substring(model, 2), search="anchor"))
    w <- which(mooringElements$anchors$name == model)
    if (1 == length(w)) {
        me <- mooringElements$anchors[w, ]
        if (!is.null(buoyancy))
            warning("ignoring supplied buoyancy, because \"", model, "\" is already in the database\n")
        if (!is.null(height))
            warning("ignoring supplied height, because \"", model, "\" is already in the database\n")
        buoyancy <- me$buoyancy
        height <- me$height
        CD <- me$CD
        source <- me$source
    } else {
        if (is.null(buoyancy)) stop("must supply buoyancy, if creating a new anchor model")
        if (is.null(height)) stop("must supply height, if creating a new anchor model")
        if (is.null(CD)) stop("must supply CD, if creating a new anchor model")
        source <- ""
    }
    rval <- list(model=model, buoyancy=buoyancy, height=height, area=0, CD=CD, depth=depth, source=source)
    class(rval) <- c("mooring", "anchor")
    rval
}                                      # anchor()
