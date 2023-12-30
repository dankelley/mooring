# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

# internal functions {
RMS <- function(x) sqrt(mean(x^2))
pluralize <- function(singular = "item", plural = NULL, n = 0L) {
    if (is.null(plural)) {
        plural <- paste0(singular, "s")
    }
    if (n == 1) paste(n, singular) else paste(n, plural)
}
# } internal functions


#' Compute mooring knockdown by a horizontal current
#'
#' The current may be a depth-independent or depth-dependent, as
#' specified by the `u` argument.  The computation is iterated at most
#' `niteration` times until the RMS deviation between computed z
#' values falls below the product of `convergenceCriterion` and wire
#' length. The returned result has an attribute named `u` that holds
#' the value of that argument, and this is how a later call to
#' [plot.mooring()] is able to display a velocity profile; see
#' \dQuote{Examples} 2 and 3.
#'
#' @param m an object of the `"mooring"` class, usually created with
#' [discretise()].
#'
#' @template uTemplate
#'
#' @param niteration integer value setting maximum number of
#' iterations that will be taken in order to achieve convergence (see
#' also `convergenceCriterion`).
#'
#' @param convergenceCriterion numeric value giving a convergence
#' criterion to stop iterating the solution.  If the RMS difference in
#' z values between iterations falls below the product of
#' `convergenceCriterion` and the water depth, then the iteration loop
#' is stopped early. Otherwise, once `niteration` iterations are
#' permitted, a warning is issued.  (Setting `debug` to a positive
#' value will cause information about each iteration to be printd.)
#'
#' @template debugTemplate
#'
#' @return a new `mooring` object representing the deformed mooring,
#' with `x` and `z` values updated (and original values saved as `x0`
#' and `z0`). In addition, the stress and angle of each element is
#' stored in fields named `tau` and `phi`, the latter in radians.
#' Attributes are added to the object to describe the solution in more
#' detail.  The `u` attribute stores the value supplied to
#' `knockdown()`, and `waterDepth' stores the water depth that was
#' supplied to the [anchor()] call.  An overview of the iterative
#' solution is provided in the `iterationCount` and `iterationChange`
#' attributes, which store the number of iterations used in the
#' computation, and the RMS change in `z` over the final iteration.
#'
#' @examples
#' # Illustrate importance of drag on the wire.
#' library(mooring)
#' m <- mooring(anchor(depth = 100), wire(length = 80), float("16in Viny"))
#' md <- discretise(m)
#'
#' # Example 1: no current
#' plot(md)
#'
#' # Example 2: uniform 1 m/s (approx. 2 knot) current
#' par(mfrow = c(1, 2))
#' k1 <- knockdown(md, u = 1)
#' plot(k1, which = "velocity")
#' plot(k1)
#'
#' # Example 3: 1 m/s current at surface, decaying exponentially below
#' k2 <- knockdown(md, u = function(depth) 1.0 * exp(-depth / 30))
#' par(mfrow = c(1, 2))
#' plot(k2)
#' plot(k2, which = "velocity")
#'
#' # Example 4: as Example 3, but show knockdown and tension
#' # The red dashed line in the tension plot indicates the
#' # anchor weight.
#' k2 <- knockdown(md, u = function(depth) 1.0 * exp(-depth / 30))
#' par(mfrow = c(1, 2))
#' plot(k2, which = "knockdown")
#' plot(k2, which = "tension")
#'
#' @importFrom graphics grid
#' @importFrom utils tail
#' @export
#' @author Dan Kelley
knockdown <- function(m, u = 1, niteration = 30, convergenceCriterion = 1e-4, debug = 0L) {
    debug <- max(0L, as.integer(debug))
    n <- length(m)
    # check for well-formed parameters
    if (!is.mooring(m)) {
        stop("only works for objects created by mooring()")
    }
    if (n < 3L) {
        stop("mooring must have 2 or more elements")
    }
    if (!inherits(m[[n]], "anchor")) {
        stop("the bottom element of a mooring must be created with anchor()")
    }
    if (!is.null(attr(m, "u"))) {
        stop("cannot apply knockdown() to the result of a previous call")
    }
    if (is.null(attr(m, "discretised"))) {
        warning("accuracy is higher if discretise() is used before knockdown()\n")
    }
    # rename x,z into x0,z0 for the stagnant (u=0) case
    for (i in seq_len(n)) {
        m[[i]]$x0 <- m[[i]]$x
        m[[i]]$z0 <- m[[i]]$z
    }
    # start actual calculation, which relies on buoyancy B and drag, D.
    waterDepth <- m[[length(m)]]$depth
    tau <- vector("numeric", n)
    phi <- vector("numeric", n)
    zold <- z(m)
    iterationChange <- NA
    for (iteration in seq_len(niteration)) {
        iterationCount <- iteration
        B <- 9.8 * buoyancy(m) # note conversion from kg to N
        D <- drag(m, u)
        # Next two are Equation 5 in the Mooring Model vignette.
        tau[1] <- sqrt(D[1]^2 + B[1]^2)
        phi[1] <- atan2(D[1], B[1])
        if (debug) {
            cat("tau[1]=", tau[1], ", phi[1]=", phi[1], "\n")
        }
        # Next block, run only if more than 2 elements, computes rest of tau and phi
        # values, using Equation 8 in the Mooring Model vignette.
        # For tension at bookmark B1c, see bookmarks B1a and B1b.
        if (n > 2L) {
            for (i in seq(2L, n - 1L)) {
                tau[i] <- sqrt((D[i] + tau[i - 1] * sin(phi[i - 1]))^2 + (B[i] + tau[i - 1] * cos(phi[i - 1]))^2) # bookmark B1c
                phi[i] <- atan2(D[i] + tau[i - 1] * sin(phi[i - 1]), B[i] + tau[i - 1] * cos(phi[i - 1]))
            }
        }
        # carry tension and angle through mooring (just for plotting; not used in calculations)
        tau[n] <- tau[n - 1L]
        phi[n] <- phi[n - 1L]
        # Clip the angle (do not allow it to run "inside" the sediment)
        phi <- ifelse(phi > pi / 2, pi / 2, phi)

        # Compute position from bottom up, starting at x=0 and z=-waterDepth
        # FIXME: save tension in object
        m[[n]]$phi <- phi[n - 1] # does this matter? Is it ever used?
        m[[n]]$x <- 0
        nm <- length(m)
        m[[n]]$z <- if (inherits(m[[nm]], "anchor")) {
            -waterDepth + m[[nm]]$height
        } else {
            -waterDepth
        }
        mooringDebug(debug, "Before knockdown, m[[1]]$z=", m[[1]]$z, ", m[[", n, "]]$z=", m[[n]]$z, "\n", sep = "")
        m[[n]]$tau <- tau[n]
        for (i in seq(n - 1L, 1L, -1L)) {
            m[[i]]$phi <- phi[i]
            m[[i]]$tau <- tau[i]
            m[[i]]$x <- m[[i + 1]]$x + m[[i]]$height * sin(m[[i]]$phi)
            m[[i]]$z <- m[[i + 1]]$z + m[[i]]$height * cos(m[[i]]$phi)
        }
        if (debug) {
            cat("iteration", iteration, " had first few data as follows\n")
            print(head(data.frame(D = D, PHI = phi, tau = tau, x = x(m), z = z(m)), 4), digits = 4)
        }
        mooringDebug(
            debug,
            sprintf("After knockdown, m[[1]]$z=%.5g m, and m[[%d]]$z=%.5g\n", m[[1]]$z, n, m[[n]]$z)
        )
        ztop <- m[[1]]$z
        if (ztop > 0) {
            warning(sprintf("mooring line too long for depth (top element %.2f m in air); expect odd results", ztop))
        }
        if (debug) {
            cat(sprintf("RMS z change: %.4gm\n", RMS(z(m) - zold)))
        }
        iterationChange <- RMS(z(m) - zold)
        if (iterationChange < convergenceCriterion * waterDepth) {
            break
        }
        zold <- z(m)
    }
    if (iterationChange >= convergenceCriterion * waterDepth) {
        warning(sprintf(
            "convergence not achieved in %s (RMS z difference was %.4gm, so exceeding convergenceCriterion*waterDepth value of %.4gm)\n",
            pluralize("iteration", n = niteration), iterationChange, convergenceCriterion * waterDepth
        ))
    }
    class(m) <- "mooring"
    attr(m, "u") <- u
    attr(m, "iterationCount") <- iterationCount
    attr(m, "iterationChange") <- iterationChange
    m
} # knockdown()
