# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Drag on mooring elements
#'
#' This looks up element areas with [area()] and drag
#' coefficients with [CD()], then computes drag
#' force (in Newtons) with
#' \eqn{(1/2)*area*rho*CD*u^2}{(1/2)*area*rho*CD*u^2}
#' and divides by `g` to get a mass equivalance, which
#' is returned.
#'
#' Although fluid density `rho` and `g` are parameters to this
#' function, the default values are likely to be used in all
#' practical oceanographic calculations, because drag coefficient
#' is not known to three digits.
#'
#' @template meTemplate
#'
#' @template uTemplate
#'
#' @template rhoTemplate
#'
#' @param g numeric value of the acceleration due to gravity, with default
#' being 9.8 m/s^2.
#'
#' @return `drag` returns a numeric vector of horizontal drag "force" (really, force
#' divided by gravitational acceleration), expressed in kg.
#'
#' @export
#'
#' @author Dan Kelley
drag <- function(m, u, rho = 1027, g = 9.8) {
    if (length(rho) > 1L && length(rho) != length(m)) {
        stop("length of rho, ", length(rho), " must match length of m, ", length(m))
    }
    uSquared <- if (is.function(u)) sapply(depth(m), u)^2 else u^2
    0.5 * area(m) * rho * CD(m) * uSquared / g
}
