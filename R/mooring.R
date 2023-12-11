# vim:spell:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

###################
# 1. overall docs #
###################

#' mooring: A Package for Analysing Oceanographic Moorings
#'
#' The mooring package provides functions for working with
#' oceanographic moorings.  The example provides a good
#' starting point for learning, and the documentation
#' for the key function, [mooring()], provides the
#' next logical step in learning, after which the
#' vignettes should be consulted.
#'
#' @examples
#' library(mooring)
#' # Illustrate the deformation of a 100-m mooring in a 0.5 m/s
#' # (roughly 1 knot) current. Buoyancy is provided with a float
#' # of diameter 20 inches.
#' m <- mooring(anchor(depth = 120), wire(length = 100), float("HMB 25"))
#' par(mfrow = c(1, 3))
#' plot(m)
#' # Must discretise the wire portion to resolve the shape.
#' md <- discretise(m)
#' mdk <- knockdown(md, u = 0.5)
#' plot(mdk)
#'
#' @docType package
#'
#' @name mooring-package
NULL

#' Dataset of wire, chain, and float properties
#'
#' This dataset was constructed from the `mdcodes.mat` (Dewey 1999, 2021).
#' This is provided for use by [wire()], [chain()] and [float()].
#' Users will seldom need to consult this table directly, but those that
#' do should note that Dewey's lengths in cm have been converted to m.
#'
#' @name mooringElements
#' @docType data
#'
#' @usage data(mooringElements)
#'
#' @examples
#' library(mooring)
#' data(mooringElements)
#' mooringElements
#'
#' @references
#' Dewey, Richard K. "Mooring Design & Dynamics-a Matlab® Package for
#' Designing and Analyzing Oceanographic Moorings." Marine Models, vol. 1, no. 1
#' (December 1, 1999): 103–57. https://doi.org/10.1016/S1369-9350(00)00002-X
#'
#' Dewey, Richard. "Mooring Design and Dynamics:
#' A Matlab Package for Designing and Testing
#' Oceanographic Moorings And Towed Bodies."
#' Accessed May 15, 2021.
#' http://canuck.seos.uvic.ca/rkd/mooring/mdd/mdd.php
#' http://canuck.seos.uvic.ca/rkd/mooring/moordyn.php
NULL

###################
# 2. setup        #
###################

g <- 9.81

#' Detect whether an object is a mooring
#'
#' An object is a mooring if it is a list that has more than one element, and
#' if each element inherits from the `"mooring"` class.  For example, the
#' output of [mooring()] is a mooring, but the output of [anchor()] is not.
#' This function is mainly designed for use within the package that that, e.g.
#' [knockdown()] will produce an error if its first argument is not a mooring.
#'
#' @param m an object to be tested
#'
#' @export
isMooring <- function(m = NULL) {
    is.list(m) && length(m) > 1 && all(sapply(m, function(mi) inherits(mi, "mooring")))
}

#' Create a mooring
#'
#' Assemble elementary components into a mooring string.
#' The first argument must be an anchor, created
#' with [anchor()].  These are stored in reverse order
#' in the return value (see Example 1).
#'
#' @param ... two or more elementary objects, as created by
#' [anchor()], [release()], [chain()],
#' [wire()], [connector()], [instrument()], [misc()] or [float()].
#'
#' @examples
#' # Example 1: most basic case: anchor, line, float.
#' library(mooring)
#' m <- mooring(anchor(depth = 100), wire(length = 80), float())
#' m # whole-mooring overview
#' head(m, 1) # float overview
#'
#' # Example 2: compare a simple mooring (with inadequate
#' # buoyancy) with a stiffer one that has extra buoyancy
#' # near an instrument.  Note that the stiffness is
#' # concentrated in the depth region that is being sampled
#' # with the instrument (a "microcat").
#' A <- anchor(depth = 150)
#' W <- function(l) wire("3/8in wire/jack", length = l)
#' MC <- instrument("SBE37 microcat clamp-on style")
#' BUB <- float("BUB 2x17in glass")
#' top <- float("Trpl 16in Viny")
#' m1 <- mooring(A, W(45), W(5), MC, W(5), W(80), top)
#' m2 <- mooring(A, W(45), BUB, W(5), MC, W(5), BUB, W(80), top)
#' m1k <- m1 |>
#'     discretise() |>
#'     knockdown(u = 0.5)
#' m2k <- m2 |>
#'     discretise() |>
#'     knockdown(u = 0.5)
#' plot(m1k)
#' # Show stiffened mooring in red
#' lines(x(m2k), depth(m2k), col = 2, lwd = 2)
#' points(x(m2k, skipWire = TRUE), depth(m2k, skipWire = TRUE), col = 2, pch = 20)
#' type <- sapply(m2k, \(i) class(i)[2])[!isWire(m2k)]
#' typeAbbrev <- unname(sapply(type, \(t)
#' switch(t,
#'     float = "F",
#'     instrument = "I",
#'     anchor = "A"
#' )))
#' text(x(m2k, skipWire = TRUE), depth(m2k, skipWire = TRUE), typeAbbrev, col = 2, pos = 2)
#'
#' @export
#'
#' @author Dan Kelley
mooring <- function(...) {
    dots <- list(...)
    n <- length(dots)
    if (n < 3L) {
        stop("need 2 or more arguments")
    }
    if (!inherits(dots[[1]], "anchor")) {
        stop("first argument must be created with anchor()")
    }
    w <- which(sapply(dots, function(x) !inherits(x, "mooring")))
    if (length(w)) {
        stop("these are the indices of elements that are not of class \"mooring\": ", paste(w, collapse = " "))
    }
    w <- which(2 != sapply(dots, function(x) length(class(x))))
    if (length(w)) {
        stop("these are the indices of elements that are not elementary: ", paste(w, collapse = " "))
    }
    # All checks seem OK, so reverse parameters and create the return value.
    rval <- rev(dots)
    depth <- rval[[n]]$depth # NOTE: only anchor() objects have this, but we know we have one
    height <- rev(cumsum(sapply(rev(rval), function(x) x$height)))
    # bookmark B1a: same as B1b and analogous t0 B1c {{{
    b <- buoyancy(rval)[-n]
    tau <- cumsum(b)
    tau <- c(tau, tau[n - 1]) # repeat tension across anchor (for plot labelling; not used for calculations)
    # }}}
    x0 <- 0
    alongBottom <- 0L
    for (i in seq_along(rval)) {
        z <- height[i] - depth
        # If the mooring outweighs the flotation, run some of it along the bottom.
        if (z < (-depth)) {
            alongBottom <- alongBottom + 1L
            z <- -depth
            x0 <- x0 + height[i]
        }
        rval[[i]]$x <- x0
        rval[[i]]$z <- z # z is at the *top* of the element
        rval[[i]]$tau <- tau[i]
    }
    if (alongBottom) {
        warning("insufficient mooring buoyancy; placed ", alongBottom, " elements on the bottom")
    }
    class(rval) <- "mooring"
    attr(rval, "waterDepth") <- depth
    rval
} # mooring()

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

#' Print a debugging message
#'
#' A function used for debugging within the package.
#'
#' @param debug an integer that controls debugging. If this is positive,
#' then the other arguments are given to [cat()], to print a message. Note
#' that no newline is printed, unless included in the \code{...} arguments.
#'
#' @param v a value to be displayed.
#'
#' @param ... optional extra values to be displayed.
#'
#' @param overview logical value indicating whether to summarize `v` (ignoring
#' `...`), by displaying its name and some of its values.
#'
#' @param round either FALSE, for no rounding, or an integer, indicating the
#' rounding to be used, via a call to [round()]. This applies only to the case
#' where `overview` is TRUE.
#'
#' @importFrom utils head tail
#'
#' @export
#'
#' @author Dan Kelley
mooringDebug <- function(debug, v, ..., overview = FALSE, round = FALSE) {
    debug <- as.integer(debug)
    if (debug > 0L) {
        if (overview) {
            msg <- paste(deparse(substitute(expr = v, env = environment())), " ", sep = "")
            if (is.numeric(round)) {
                v <- round(v, as.integer(round))
            }
            n <- length(v)
            if (n > 1) {
                msg <- paste(msg, "[1:", n, "] ", sep = "")
            }
            if (n < 7) {
                msg <- paste(msg, paste(v, collapse = " "), "\n", sep = "")
            } else {
                msg <- paste(msg, paste(head(v, 3), collapse = " "), " ... ", paste(tail(v, 3), collapse = " "), "\n", sep = "")
            }
            cat(msg)
        } else {
            cat(v, ...)
        }
    }
    invisible(NULL)
}

#' Summarize a mooring
#'
#' @param object a mooring object, created by [mooring()].
#'
#' @param ... ignored.
#'
#' @examples
#' library(mooring)
#' # Simple case
#' m <- mooring(anchor(depth = 100), wire(length = 80), float("HMB 20"))
#' summary(m)
#' # Illustrate how it collects wire subintervals
#' md <- discretise(m)
#' mdk <- knockdown(md, 0.5)
#' summary(mdk)
#'
#' @export
#'
#' @author Dan Kelley
summary.mooring <- function(object, ...) {
    m <- object # use a more useful name
    if (!isMooring(m)) {
        stop("only works for objects created by mooring()")
    }
    n <- length(m)
    lastWasWire <- FALSE
    wireLength <- 0
    iWireStart <- 0
    for (i in seq_len(n)) {
        mi <- m[[i]]
        if (inherits(mi, "wire")) {
            if (!lastWasWire) {
                iWireStart <- i
            }
            lastWasWire <- TRUE
            wireLength <- wireLength + mi$height
        } else {
            if (lastWasWire) {
                # fake an element (and blank out the location)
                W <- m[[iWireStart]]
                W$height <- wireLength
                # cat("iWireStart=", iWireStart, "\n")
                print(W)
            }
            lastWasWire <- FALSE
            wireLength <- 0
            print(mi)
        }
    }
}

#' Access something in a mooring
#'
#' Retrieves values from (a) a mooring element, as created with
#' [float()] or a similar function, or (b) a whole mooring, as created
#' with [mooring()].
#'
#' @template mTemplate
#'
#' @param i either (a) an integer specifying index of item to be returned, or
#' (b) a character string. The first case is used to look up components of
#' a mooring. The second case requires `i` to be `"area"`, `"buoyancy"`,
#' `"CD"`, or `"height"`, and the result is a single value if `m` is
#' an elementary object (example 1) or a whole mooring (example 2).
#'
#' @examples
#' library(mooring)
#' F <- float("HMB 20")
#' F["buoyancy"]
#' m <- mooring(anchor(depth = 120), wire(length = 100), F)
#' m["buoyancy"]
#'
#' @export
#'
#' @author Dan Kelley
`[.mooring` <- function(m, i) {
    known <- c("area", "buoyancy", "CD", "height")
    if (isMooring(m)) {
        if (is.numeric(i)) {
            # message("m[i] with i=",paste(i, collapse=" "))
            i <- subset(i, 0L < i & i <= length(m))
            # message(" >> i=",paste(i, collapse=" "))
            um <- unclass(m)
            rval <- lapply(i, function(mi) um[[mi]])
            class(rval) <- class(m)
            rval
        } else {
            # message("m char")
            if (i %in% known) {
                sapply(m, function(mi) mi[[i]])
            } else {
                stop("\"", i, "\" not handled; try one of: \"", paste(known, collapse = "\", \""), "\"")
            }
        }
    } else {
        if (length(class(m)) != 2) {
            stop("only works for a mooring, or an element of a mooring")
        }
        if (is.numeric(i)) {
            stop("integer lookup is not permitted for elementary objects")
        } else {
            # message("e char")
            if (i %in% known) {
                unclass(m)[[i]]
            } else {
                stop("'", i, "' not handled; try one of: '", paste(known, collapse = "', '"), "'")
            }
        }
    }
}
