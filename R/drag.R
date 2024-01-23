# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Drag on mooring elements
#'
#' This looks up element areas with [area()] and drag
#' coefficients with [CD()], then computes drag
#' force (in Newtons) with
#' \eqn{(1/2)*area*rho*CD*u^2}{(1/2)*area*rho*CD*u^2}.
#'
#' Although fluid density `rho` is a parameter of this
#' function, the default value is likely to be used in all
#' practical oceanographic calculations, because neither
#' the drag coefficient, `CD` nor the current is easily
#' constrained to a corresponding tolerance.
#'
#' @template meTemplate
#'
#' @template uTemplate
#'
#' @param phi passed to [area()], so see that function's documentation
#' for the somewhat subtle details.
#'
#' @template rhoTemplate
#'
#' @return `drag` returns a numeric vector of horizontal drag force in
#' Newtons.
#'
#' @export
#'
#' @author Dan Kelley
drag <- function(m, u, phi = TRUE, rho = 1027) {
    if (length(rho) > 1L && length(rho) != length(m)) {
        stop("length of rho, ", length(rho), " must match length of m, ", length(m))
    }
    uSquared <- if (is.function(u)) sapply(depth(m), u)^2 else u^2
    0.5 * area(m, phi = phi) * rho * CD(m) * uSquared
}
