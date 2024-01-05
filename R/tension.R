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
#' m <- mooring(anchor(), wire(length = 100), float("HMB 20"), waterDepth = 120)
#' md <- segmentize(m)
#' mdk <- knockdown(md, u = 0.5)
#' depth <- depth(mdk)
#' # The right-hand panel indicates the anchor limit in red.
#' par(mfrow = c(1, 2))
#' plot(tension(mdk), depth, ylim = rev(range(depth)), type = "l")
#' draw(mdk, which = "tension")
#'
#' @export
#'
#' @author Dan Kelley
tension <- function(m, stagnant = FALSE) {
    if (!is.mooring(m)) {
        stop("only works for objects created by mooring()")
    }
    elements <- m@elements
    n <- length(elements)
    # message("tension 1 (n=", n, ")")
    if (stagnant || all(x(m) == 0.0)) {
        # message("tension 2a1")
        n <- length(elements)
        # message("tension 2a2")
        # bookmark B1b: same as B1a and analogous to B1c {{{
        b <- buoyancy(m)[-n]
        # message("tension 2a3")
        tau <- cumsum(b)
        # message("tension 2a4")
        # repeat tension across anchor (for plot labelling; not used for calculations)
        rval <- c(tau, tau[n - 1])
        # message("tension 2a5")
        # }}}
    } else {
        # message("tension 2b1")
        rval <- sapply(m@elements, \(e) e@tau)
        # message("tension 2b2")
    }
    rval
} # tension()
