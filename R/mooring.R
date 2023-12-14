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

#g <- 9.81


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
#' type <- sapply(m2k, \(i) class(i)[2])[!is.wire(m2k)]
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
        stop("need 3 or more arguments")
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
