# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4


#' Compute mooring knockdown by a horizontal current
#'
#' The current may be a depth-independent or depth-dependent, as
#' specified by the `u` argument.  The computation is iterated at most
#' `niteration` times until the RMS deviation between computed z
#' values falls below the product of `convergenceCriterion` and wire
#' length. The returned result has an attribute named `u` that holds
#' the value of that argument, and this is how a later call to
#' [plot()] is able to display a velocity profile; see
#' Examples 2 and 3.
#'
#' @param m an object of the `"mooring"` class, usually created with
#' [segmentize()].
#'
#' @template uTemplate
#'
#' @param convergence,maxiteration criteria for stopping the iteration process.
#' If the root-mean-squared difference in element angles
#' drops below this `convergence` degrees, then convergence is declared and the iterative
#' solution process stops. If not, then the iteration stops after `maxiteration`
#' passes (and a warning is issued).  In any case, the actual number of
#' iterations is stored in an attribute named `iterations`, the RMS
#' angle difference (in degrees) is stored as `RMSAngleChange`,
#' and the RMS difference in element depth (in m) is stored
#' `RMSDepthChange`.
#'
#' @template debugTemplate
#'
#' @return `knockdown` returns a `mooring` object representing the
#' deformed mooring, with `x` and `z` values updated, with the
#' original values being saved as `x0` and `z0`. In addition, the
#' stress and angle of each element is stored in fields named `tau`
#' and `phi`, the latter in radians. Attributes are added to the
#' object to describe the solution in more detail.  The `u` attribute
#' stores the value supplied to `knockdown()`, and `waterDepth` stores
#' the water depth that was supplied to the [anchor()] call.  An
#' overview of the iterative solution is provided in the
#' attributes (see `convergence`).
#'
#' @examples
#' # Illustrate importance of drag on the wire.
#' library(mooring)
#' m <- mooring(anchor(), wire(length = 30), float("16in Viny"), waterDepth = 50)
#' ms <- segmentize(m)
#'
#' # Example 1: no current
#' plot(ms)
#'
#' # Example 2: uniform 0.5 m/s (approx. 1 knot) current
#' par(mfrow = c(1, 2))
#' k1 <- knockdown(ms, u = 0.5)
#' plot(k1, which = "velocity")
#' plot(k1)
#'
#' # Example 3: 0.5 m/s surface current, exponential decay below
#' k2 <- knockdown(ms, u = function(depth) 0.5 * exp(-depth / 30))
#' par(mfrow = c(1, 2))
#' plot(k2)
#' plot(k2, which = "velocity")
#'
#' # Example 4: as Example 3, but show knockdown and tension
#' # The red dashed line in the tension plot indicates the
#' # anchor weight.
#' par(mfrow = c(1, 2))
#' plot(k2, which = "knockdown")
#' plot(k2, which = "tension")
#'
#' @importFrom graphics grid
#' @importFrom utils tail
#' @export
#' @author Dan Kelley
knockdown <- function(m, u = 1, convergence = 0.1, maxiteration = 30, debug = 0L) {
    debug <- max(0L, as.integer(debug))
    # check for well-formed parameters
    if (!is.mooring(m)) {
        stop("only works for objects created by mooring()")
    }
    n <- length(m@elements)
    if (n < 3L) {
        stop("mooring must have 2 or more elements")
    }
    if (!is.anchor(m@elements[[n]])) {
        stop("the bottom element of a mooring must be created with anchor()")
    }
    # rename x,z into x0,z0 for the stagnant (u=0) case
    for (i in seq_len(n)) {
        m@elements[[i]]@x0 <- m@elements[[i]]@x
        m@elements[[i]]@z0 <- m@elements[[i]]@z
    }
    # start actual calculation, which relies on buoyancy B and drag, D.
    waterDepth <- m@waterDepth
    tau <- vector("numeric", n)
    phi <- rep(0.0, length.out = n)
    zold <- z(m)
    angleold <- angle(m)
    RMSChangeAngle <- NA # in degrees, even though phi is in radians
    RMSChangeDepth <- NA
    for (iteration in seq_len(maxiteration)) {
        mooringDebug(debug, "Iteration ", iteration, " (of possibly ", maxiteration, ")\n", sep="")
        iterationCount <- iteration
        B <- 9.81 * buoyancy(m) # note conversion from kg to N
        D <- drag(m, u)
        # Next two are Equation 5 in the Mooring Model vignette.
        tau[1] <- sqrt(D[1]^2 + B[1]^2)
        phi[1] <- atan2(D[1], B[1])
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
        zm <- z(m)
        if (debug) {
            cat("Initially, first/last few data are as follows\n")
            look <- c(1, 2, n - 1, n)
            print(data.frame(angle = 180 / pi * phi, z = zm)[look, ],
                digits = 4
            )
        }
        # Compute position from bottom up, starting at x=0 and z=-waterDepth
        m@elements[[n]]@phi <- phi[n - 1] # does this matter? Is it ever used?
        m@elements[[n]]@x <- 0
        m@elements[[n]]@z <- -waterDepth + m@elements[[n]]@height
        m@elements[[n]]@tau <- tau[n]
        for (i in seq(n - 1L, 1L, -1L)) {
            m@elements[[i]]@phi <- phi[i]
            m@elements[[i]]@tau <- tau[i]
            m@elements[[i]]@x <- m@elements[[i + 1]]@x + m@elements[[i]]@height * sin(m@elements[[i]]@phi)
            m@elements[[i]]@z <- m@elements[[i + 1]]@z + m@elements[[i]]@height * cos(m@elements[[i]]@phi)
        }
        ztop <- m@elements[[1]]@z
        if (ztop > 0) {
            warning(sprintf("mooring line too long for depth (top element %.2f m in air); expect odd results", ztop))
        }
        zm <- z(m)
        anglem <- angle(m)
        RMSChangeDepth <- RMS(zm - zold)
        RMSChangeAngle <- 180 / pi * RMS(anglem - angleold)
        if (debug) {
            cat("After calculation, first/last few data are as follows\n")
            look <- c(1, 2, n - 1, n)
            print(data.frame(angle = 180 / pi * phi, z = zm)[look, ],
                digits = 4
            )
        }
        if (debug) {
            cat(sprintf("RMS angle change: %.4g deg\n", 180 / pi * RMSChangeAngle))
            cat(sprintf("RMS depth change: %.4g m\n", RMSChangeDepth))
        }
        if (RMSChangeAngle < pi / 180 * convergence) {
            break
        }
        zold <- zm
        angleold <- anglem
        mooringDebug(debug, "\n")
    }
    if (RMSChangeAngle >= pi / 180 * convergence) {
        warning(sprintf(
            "convergence not achieved in %s; RMS angle change %.4g deg, RMS depth change %.4g m\n",
            pluralize("iteration", n = maxiteration), 180 / pi * RMSChangeAngle, RMSChangeDepth
        ))
    }
    m@u <- u
    attr(m, "iteration") <- iterationCount
    attr(m, "RMSChangeAngle") <- 180 / pi * RMSChangeAngle
    attr(m, "RMSChangeDepth") <- RMSChangeDepth
    m
} # knockdown()
