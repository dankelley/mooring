# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Tension between mooring elements
#'
#' The first element (for the anchor) is repeated, so that the
#' length of the returned result matches the length of `m`.
#'
#' @template mTemplate
#'
#' @template stagnantTemplate
#'
#' @return a numeric vector of tension, in kg.
#'
#' @examples
#' library(mooring)
#' m <- mooring(anchor(depth = 120), wire(length = 100), float("HMB 20"))
#' md <- discretise(m)
#' mdk <- knockdown(md, u = 0.5)
#' depth <- depth(mdk)
#' # Next is handled better by plot.mooring(mdk,which="tension")
#' plot(tension(mdk), depth, ylim = rev(range(depth)), type = "l")
#'
#' @export
#'
#' @author Dan Kelley
tension <- function(m, stagnant = FALSE) {
    if (!isMooring(m)) {
        stop("only works for objects created by mooring()")
    }
    if (stagnant || all(x(m) == 0.0)) {
        n <- length(m)
        # bookmark B1b: same as B1a and analogous to B1c {{{
        b <- buoyancy(m)[-n]
        tau <- cumsum(b)
        # repeat tension across anchor (for plot labelling; not used for calculations)
        c(tau, tau[n - 1])
        # }}}
    } else {
        sapply(m, function(mi) mi$tau)
    }
} # tension()
