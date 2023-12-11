# vim:spell:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Print a mooring
#'
#' @param x an object of the `"mooring"` class.
#'
#' @param ... optional arguments.  If this includes `debug`, and if that holds
#' a number greater than zero, then some debugging information is printed.
#'
#' @examples
#' library(mooring)
#' m <- mooring(anchor(depth = 100), wire(length = 80), float("HMB 20"))
#'
#' @export
#'
#' @author Dan Kelley
print.mooring <- function(x, ...) {
    debug <- if ("debug" %in% names(list(...))) list(...)$debug else 0L
    mooringDebug(debug, "print.mooring() {\n", sep = "")
    elementary <- 2 == length(class(x))
    n <- if (elementary) 1L else length(x)
    if (elementary || n == 1L) {
        prefix <- ""
    } else {
        if (is.null(attr(x, "discretised"))) {
            cat("Mooring with", n, "elements, listed from the top down:\n")
        } else {
            if (is.null(attr(x, "u"))) {
                cat("Discretised mooring with", n, "elements, listed from the top down:\n")
            } else {
                cat("Discretised, knocked-over mooring with", n, "elements, listed from the top down:\n")
            }
        }
        prefix <- "  "
    }
    # The'lastWas* variables keep track of repeats, e.g. as created by discretise().
    # This scheme will not work if a mooring is contructed with wire or chain elements
    # that are not joined by a connector, but that should not happen if the mooring
    # reflects reality.  If this poses a problem, we could also look at the group
    #' part of the item.
    lastWasChain <- FALSE
    lastWasWire <- FALSE
    i <- 1L
    while (i <= n) {
        xi <- if (elementary) x else x[[i]]
        mooringDebug(debug, "i=", i, " class=", paste(class(xi), collapse = ","), "\n", sep = "")
        if (inherits(xi, "anchor")) {
            cat(sprintf(
                "%s%d: \"%s\" anchor, %gkg, height %gm, in %gm water depth\n",
                prefix, i, xi$model, xi$buoyancy, xi$height, xi$depth
            ), sep = "")
            lastWasChain <- lastWasWire <- FALSE
            i <- i + 1L
        } else if (inherits(xi, "chain")) {
            # See if there are more chain elements following this.
            mooringDebug(debug, "a chain; n=", n, ", n-i=", n - i, "\n")
            count <- 1L
            while (count <= (n - i)) {
                if (!inherits(x[[i + count]], "chain")) {
                    break
                }
                count <- count + 1L
            }
            #> message("chain count: ", count)
            if (count == 1L) {
                cat(sprintf(
                    "%s%d: \"%s\" chain, %gkg, length %gm, area %gm^2\n",
                    prefix, i, xi$model,
                    xi$buoyancy,
                    xi$height,
                    xi$area
                ), sep = "")
            } else {
                cat(sprintf(
                    "%s%d-%d: \"%s\" chain, %gm, length %gm, width %gm\n",
                    prefix, i, i + count - 1L, xi$model,
                    xi$buoyancy,
                    xi$height,
                    xi$area
                ), sep = "")
            }
            i <- i + count # account for skipped-over elements
        } else if (inherits(xi, "connector")) {
            cat(sprintf(
                "%s%d: \"%s\" connector, %gkg, height %gm, area %gm^2\n",
                prefix, i, xi$model, xi$buoyancy, xi$height, xi$area
            ), sep = "")
            lastWasChain <- lastWasWire <- FALSE
            i <- i + 1L
        } else if (inherits(xi, "float")) {
            cat(sprintf(
                "%s%d: \"%s\" float, %gkg, height %gm, area %gm^2\n",
                prefix, i, xi$model, xi$buoyancy, xi$height, xi$area
            ), sep = "")
            lastWasChain <- lastWasWire <- FALSE
            i <- i + 1L
        } else if (inherits(xi, "instrument")) {
            cat(sprintf(
                "%s%d: \"%s\" instrument, %gkg, area %gm^2\n",
                prefix, i, xi$model, xi$buoyancy, xi$area
            ), sep = "")
            lastWasChain <- lastWasWire <- FALSE
            i <- i + 1L
        } else if (inherits(xi, "misc")) {
            cat(sprintf(
                "%s%d: \"%s\" misc, %gkg, height %gm, area %gm^2\n",
                prefix, i, xi$model, xi$buoyancy, xi$height, xi$area
            ), sep = "")
            lastWasChain <- lastWasWire <- FALSE
            i <- i + 1L
        } else if (inherits(xi, "release")) {
            cat(sprintf(
                "%s%d: \"%s\" release, %gkg, height %gm, area %gm^2\n",
                prefix, i, xi$model, xi$buoyancy, xi$height, xi$area
            ), sep = "")
            lastWasChain <- lastWasWire <- FALSE
            i <- i + 1L
        } else if (inherits(xi, "wire")) {
            # See if there are more wire elements following this.
            mooringDebug(debug, "a wire; n=", n, ", n-i=", n - i, "\n")
            count <- 1L
            while (count <= (n - i)) {
                if (!inherits(x[[i + count]], "wire")) {
                    break
                }
                count <- count + 1L
            }
            #> message("wire count: ", wire)
            if (count == 1L) {
                cat(sprintf(
                    "%s%d: \"%s\" wire, %gkg, length %gm, area %gm^2\n",
                    prefix, i, xi$model,
                    xi$buoyancy,
                    xi$height,
                    xi$area
                ), sep = "")
            } else {
                cat(sprintf(
                    "%s%d-%d: \"%s\" wire, %gkg, length %gm, area %gm^2\n",
                    prefix, i, i + count - 1L, xi$model,
                    xi$buoyancy,
                    xi$height,
                    xi$area
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
