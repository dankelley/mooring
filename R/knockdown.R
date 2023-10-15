# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Compute mooring knockdown by a horizontal current
#'
#' The current may be a depth-independent or depth-dependent,
#' as specified by the `u` argument.  The returned result has
#' an attribute named `u` that holds the value of that
#' argument, and this is how a later call to [plot.mooring()]
#' is able to display a velocity profile; see
#' \dQuote{Examples} 2 and 3.
#'
#' @param m an object of the `"mooring"` class, usually created with
#' [discretise()].
#'
#' @template uTemplate
#'
#' @template debugTemplate
#'
#' @return a new `mooring` object representing the deformed mooring, with
#' `x` and `z` values updated (and original values saved as `x0` and `z0`).
#'
#' @examples
#' # Illustrate importance of drag on the wire.
#' library(mooring)
#' m <- mooring(anchor(depth=100), wire(length=80), float("HMB 20"))
#' md <- discretise(m)
#'
#' # Example 1: no current
#' plot(md)
#'
#' # Example 2: uniform 0.5 m/s (approx. 1 knot) current
#' par(mfrow=c(1, 2))
#' k1 <- knockdown(md, u=0.5)
#' plot(k1, which="velocity")
#' plot(k1)
#'
#' # Example 3: 0.5 m/s current at surface, decaying exponentially below
#' k2 <- knockdown(md, u=function(depth) 0.5*exp(-depth/100))
#' par(mfrow=c(1, 2))
#' plot(k2, which="velocity")
#' plot(k2)
#'
#' # Example 4: as Example 3, but show knockdown and tension
#' k2 <- knockdown(md, u=function(depth) 0.5*exp(-depth/100))
#' par(mfrow=c(1, 2))
#' plot(k2, which="knockdown")
#' plot(k2, which="tension")
#'
#' @importFrom graphics grid
#' @importFrom utils tail
#' @export
#' @author Dan Kelley
knockdown <- function(m, u=1, debug=0L)
{
    debug <- max(0L, as.integer(debug))
    n <- length(m)
    # check for well-formed parameters
    if (n < 3L)
        stop("mooring must have 2 or more elements")
    if (!isMooring(m))
        stop("only works for objects created by mooring()")
    if (!inherits(m[[length(m)]], "anchor"))
        stop("the bottom element of a mooring must be created with anchor()")
    if (!is.null(attr(m, "u")))
        stop("cannot apply knockdown() to the result of a previous call")
    if (is.null(attr(m, "discretised")))
        warning("accuracy is better if discretise() is used first\n")
    if (is.function(u) && debug > 0L)
        warning("FIXME: u=function() case is not fully coded yet (no iteration is done)\n")
    # rename x,z into x0,z0 for the stagnant (u=0) case
    for (i in seq_len(n)) {
        m[[i]]$x0 <- m[[i]]$x
        m[[i]]$z0 <- m[[i]]$z
    }
    # start actual calculation, which relies on buoyancy B and drag, D.
    waterDepth <- m[[length(m)]]$depth
    B <- buoyancy(m)
    D <- drag(m, u)
    tau <- vector("numeric", n)
    phi <- vector("numeric", n)
    # Next two are Equation 5 in the Mooring Model vignette.
    tau[1] <- sqrt(D[1]^2 + B[1]^2)
    phi[1] <- atan2(D[1], B[1])
    if (debug)
        cat("tau[1]=", tau[1], ", phi[1]=", phi[1], "\n")
    # Next block, run only if more than 2 elements, computes rest of tau and phi
    # values, using Equation 8 in the Mooring Model vignette.
    # For tension at bookmark B1c, see bookmarks B1a and B1b.
    if (n > 2L) {
        for (i in seq(2L, n-1L)) {
            tau[i] <- sqrt((D[i]+tau[i-1]*sin(phi[i-1]))^2 + (B[i]+tau[i-1]*cos(phi[i-1]))^2) # bookmark B1c
            phi[i] <- atan2(D[i]+tau[i-1]*sin(phi[i-1]), B[i]+tau[i-1]*cos(phi[i-1]))
        }
    }
    # carry tension and angle through mooring (just for plotting; not used in calculations)
    tau[n] <- tau[n-1L]
    phi[n] <- phi[n-1L]
    # Clip the angle (do not allow it to run "inside" the sediment)
    phi <- ifelse(phi > pi/2, pi/2, phi)
    # Compute position from bottom up, starting at x=0 and z=-waterDepth
    # FIXME: save tension in object
    m[[n]]$phi <- phi[n-1] # does this matter? Is it ever used?
    m[[n]]$x <- 0
    m[[n]]$z <- -waterDepth
    m[[n]]$tau <- tau[n]
    for (i in seq(n-1L, 1L, -1L)) {
        m[[i]]$phi <- phi[i]
        m[[i]]$tau <- tau[i]
        m[[i]]$x <- m[[i+1]]$x + m[[i]]$height * sin(m[[i]]$phi)
        m[[i]]$z <- m[[i+1]]$z + m[[i]]$height * cos(m[[i]]$phi)
    }
    class(m) <- "mooring"
    attr(m, "u") <- u
    m
}                                      # knockdown()
