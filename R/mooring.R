# vim:spell:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#<OLD> #' mooring: A Package for Analysing Oceanographic Moorings
#<OLD> #'
#<OLD> #' The mooring package provides functions for working with
#<OLD> #' oceanographic moorings.  The example provides a good
#<OLD> #' starting point for learning, and the documentation
#<OLD> #' for the key function, [mooring()], provides the
#<OLD> #' next logical step in learning, after which the
#<OLD> #' vignettes should be consulted.
#<OLD> #'
#<OLD> #' @examples
#<OLD> #' library(mooring)
#<OLD> #' # Illustrate the deformation of a 100-m mooring in a 0.5 m/s
#<OLD> #' # (roughly 1 knot) current. Buoyancy is provided with a float
#<OLD> #' # of diameter 20 inches.
#<OLD> #' m <- mooring(anchor(), wire(length = 100), float("HMB 25"), waterDepth = 120)
#<OLD> #' par(mfrow = c(1, 3))
#<OLD> #' plot(m)
#<OLD> #' # Must discretise the wire portion to resolve the shape.
#<OLD> #' md <- discretise(m)
#<OLD> #' mdk <- knockdown(md, u = 0.5)
#<OLD> #' plot(mdk)
#<OLD> #'
#<OLD> #' @docType package
#<OLD> #'
#<OLD> #' @name mooring-package
#<OLD> NULL

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

# g <- 9.81


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
#' @param waterDepth numerical value giving the full depth of
#' the water column.  For a sub-surface mooring, this must exceed
#' the sum of the heights of all the mooring elements.
#'
#' @return `mooring` returns an object in the `mooring` class that
#' contains an item named `elements`, which holds the items provided
#' in the `...` argument.  Each of those items is of the
#' `mooringElement` class.
#'
#' @examples
#' # Example 1: most basic case: anchor, line, float.
#' library(mooring)
#' m <- mooring(anchor(), wire(length = 80), float(), waterDepth = 100)
#' m # whole-mooring overview
#' m@elements[[1]] # float details
#'
## # Example 2: compare a simple mooring (with inadequate
## # buoyancy) with a stiffer one that has extra buoyancy
## # near an instrument.  Note that the stiffness is
## # concentrated in the depth region that is being sampled
## # with the instrument (a "microcat").
## A <- anchor()
## W <- function(l) wire("3/8in wire/jack", length = l)
## MC <- instrument("SBE37 microcat clamp-on style")
## BUB <- float("BUB 2x17in glass")
## top <- float("Trpl 16in Viny")
## m1 <- mooring(A, W(20), W(5), MC, W(5), W(20), top, waterDepth = 80)
## m2 <- mooring(A, W(20), BUB, W(5), MC, W(5), BUB, W(20), top, waterDepth = 80)
## m1k <- m1 |>
##     discretise() |>
##     knockdown(u = 0.3)
## m2k <- m2 |>
##     discretise() |>
##     knockdown(u = 0.3)
## plot(m1k)
## # Show stiffened mooring in red
## lines(x(m2k), depth(m2k), col = 2, lwd = 2)
## points(x(m2k, skipWire = TRUE), depth(m2k, skipWire = TRUE), col = 2, pch = 20)
#'
#' @export
#'
#' @author Dan Kelley
mooring <- function(..., waterDepth = NA) {
    dots <- list(...)
    n <- length(dots)
    if (n < 3L) {
        stop("need 3 or more arguments")
    }
    if (!is.anchor(dots[[1]])) {
        stop("first argument must be created with anchor()")
    }
    w <- which(sapply(dots, \(x) !is.mooringElement(x)))
    if (length(w)) {
        stop("parameters at the following indices are not of \"mooringElement\" class: ", paste(w, collapse = " "))
    }
    # w <- which(2 != sapply(dots, \(x) length(class(x))))
    # if (length(w)) {
    #    stop("these are the indices of elements that are not elementary: ", paste(w, collapse = " "))
    # }
    # All checks seem OK, so reverse parameters and create the return value.
    # message("DAN 1 mooring.R L140")
    rval <- mooringS7(dots, waterDepth = waterDepth)
    # store with top at first, because knockdown() works in that order
    rval@elements <- rev(rval@elements)
    # message("DAN 2 mooring.R L142")
    #class(rval) <- "mooring"
    depth <- waterDepth # rval[[n]]@depth # NOTE: only anchor() objects have this, but we know we have one
    # message("AA 1")
    height <- rev(cumsum(sapply(rev(rval@elements), \(x) x@height)))
    # cat("next is height\n");print(height)
    # message("AA 2")
    # bookmark B1a: same as B1b and analogous t0 B1c {{{
    b <- buoyancy(rval)[-n] # the -n removes the anchor
    # message("AA 3")
    tau <- cumsum(b)
    # message("AA 4")
    tau <- c(tau, tau[n - 1]) # repeat tension across anchor (for plot labelling; not used for calculations)
    # message("AA 5")
    # }}}
    x0 <- 0
    alongBottom <- 0L
    for (i in seq_along(rval@elements)) {
        # message("AA 6 i=", i)
        z <- height[i] - depth
        # If the mooring outweighs the flotation, run some of it along the bottom.
        if (z < (-depth)) {
            alongBottom <- alongBottom + 1L
            z <- -depth
            x0 <- x0 + height[i]
        }
        rval@elements[[i]]@x <- x0
        rval@elements[[i]]@x0 <- x0
        rval@elements[[i]]@z <- z # z is at the *top* of the element
        rval@elements[[i]]@z0 <- z # z is at the *top* of the element
        rval@elements[[i]]@tau <- tau[i]
        rval@elements[[i]]@phi <- 0.0
    }
    if (alongBottom) {
        warning("insufficient mooring buoyancy; placed ", alongBottom, " elements on the bottom")
    }
    rval@waterDepth <- depth
    rval
} # mooring()
