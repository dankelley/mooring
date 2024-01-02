# vim:spell:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Print a mooring
#'
#' @param x an object of the `"mooring"` class, as created by [mooring()].
#'
#' @param ... optional arguments.  If this includes `debug`, and if that holds
#' a number greater than zero, then some debugging information is printed.
#'
#' @examples
#' library(mooring)
#' m <- mooring(anchor(), wire(length = 80), float("HMB 20"), waterDepth = 100)
#'
#' @export
#'
#' @aliases print.mooring
#'
#' @author Dan Kelley
`print.mooring::mooring` <- function(x, ...) {
    debug <- 0
    mooringDebug(debug, "print.mooring() {\n", sep = "")
    elementary <- is.mooringElement(x)
    n <- if (elementary) 1L else length(x@elements)
    if (elementary || n == 1L) {
        prefix <- ""
    } else {
        if (is.null(attr(x, "discretised"))) {
            cat(sprintf(
                "Mooring in %gm of water that has %d elements, listed from the top down:\n",
                x@waterDepth, n
            ))
        } else {
            if (is.null(attr(x, "u"))) {
                cat("Discretised mooring with", n, "elements, listed from the top down:\n")
            } else {
                cat("Discretised, knocked-over mooring with", n, "elements, listed from the top down:\n")
            }
        }
        prefix <- "  "
    }
    # The lastWas variables keep track of repeats, e.g. as created by discretise().
    # This scheme will not work if a mooring is constructed with wire or chain elements
    # that are not joined by a connector, but that should not happen if the mooring
    # reflects reality.  If this poses a problem, we could also look at the group
    #' part of the item.
    # lastWasChain <- FALSE
    # lastWasWire <- FALSE
    i <- 1L
    while (i <= n) {
        xi <- if (elementary) x else x@elements[[i]]
        mooringDebug(debug, "i=", i, " class=", paste(class(xi), collapse = ","), "\n", sep = "")
        if (is.anchor(xi)) {
            cat(sprintf(
                "%s%d: \"%s\" anchor, buoyancy %gkg, height %gm\n",
                prefix, i, xi@model, xi@buoyancy, xi@height
            ), sep = "")
            # lastWasChain <- lastWasWire <- FALSE
            i <- i + 1L
        } else if (is.chain(xi)) {
            # See if there are more chain elements following this.
            mooringDebug(debug, "a chain; n=", n, ", n-i=", n - i, "\n")
            count <- 1L
            while (count <= (n - i)) {
                if (!is.chain(x@elements[[i + count]])) {
                    break
                }
                count <- count + 1L
            }
            #> message("chain count: ", count)
            if (count == 1L) {
                cat(sprintf(
                    "%s%d: \"%s\" chain, buoyancy %gkg, length %gm, area %gm^2\n",
                    prefix, i, xi@model,
                    xi@buoyancy,
                    xi@height,
                    xi@area
                ), sep = "")
            } else {
                cat(sprintf(
                    "%s%d-%d: \"%s\" chain, buoyancy %gm, length %gm, width %gm\n",
                    prefix, i, i + count - 1L, xi@model,
                    xi@buoyancy,
                    xi@height,
                    xi@area
                ), sep = "")
            }
            i <- i + count # account for skipped-over elements
        } else if (is.connector(xi)) {
            cat(sprintf(
                "%s%d: \"%s\" connector, buoyancy %gkg, height %gm, area %gm^2\n",
                prefix, i, xi@model, xi@buoyancy, xi@height, xi@area
            ), sep = "")
            # lastWasChain <- lastWasWire <- FALSE
            i <- i + 1L
        } else if (is.float(xi)) {
            cat(sprintf(
                "%s%d: \"%s\" float, buoyancy %gkg, height %gm, area %gm^2\n",
                prefix, i, xi@model, xi@buoyancy, xi@height, xi@area
            ), sep = "")
            # lastWasChain <- lastWasWire <- FALSE
            i <- i + 1L
        } else if (is.instrument(xi)) {
            cat(sprintf(
                "%s%d: \"%s\" instrument, buoyancy %gkg, height %gm, area %gm^2\n",
                prefix, i, xi@model, xi@buoyancy, xi@height, xi@area
            ), sep = "")
            # lastWasChain <- lastWasWire <- FALSE
            i <- i + 1L
        } else if (is.misc(xi)) {
            cat(sprintf(
                "%s%d: \"%s\" misc, buoyancy %gkg, height %gm, area %gm^2\n",
                prefix, i, xi@model, xi@buoyancy, xi@height, xi@area
            ), sep = "")
            # lastWasChain <- lastWasWire <- FALSE
            i <- i + 1L
        } else if (is.release(xi)) {
            cat(sprintf(
                "%s%d: \"%s\" release, buoyancy %gkg, height %gm, area %gm^2\n",
                prefix, i, xi@model, xi@buoyancy, xi@height, xi@area
            ), sep = "")
            # lastWasChain <- lastWasWire <- FALSE
            i <- i + 1L
        } else if (is.wire(xi)) {
            # See if there are more wire elements following this.
            mooringDebug(debug, "a wire; n=", n, ", n-i=", n - i, "\n")
            count <- 1L
            while (count <= (n - i)) {
                if (!is.wire(x@elements[[i + count]])) {
                    break
                }
                count <- count + 1L
            }
            #> message("wire count: ", wire)
            if (count == 1L) {
                cat(sprintf(
                    "%s%d: \"%s\" wire, buoyancy %gkg, length %gm, area %gm^2\n",
                    prefix, i, xi@model,
                    xi@buoyancy,
                    xi@height,
                    xi@area
                ), sep = "")
            } else {
                cat(sprintf(
                    "%s%d-%d: \"%s\" wire, buoyancy %gkg, length %gm, area %gm^2\n",
                    prefix, i, i + count - 1L, xi@model,
                    xi@buoyancy,
                    xi@height,
                    xi@area
                ), sep = "")
            }
            i <- i + count # account for skipped-over elements
        } else {
            stop("unknown class c(\"", paste(class(xi), collapse = "\", \""), "\")")
        }
    }
    mooringDebug(debug, "} # print.mooring()\n", sep = "")
    invisible(x)
}
