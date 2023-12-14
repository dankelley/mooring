# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Clamp an Instrument on a Wire
#'
#' This changes the `height` field of an `instrument` object to 0,
#' so that it does not contribute to the overall length of the mooring.
#' Using this in deep water is unlikely to make much difference,
#' but it is handy for shallow-water cases, if the device was
#' clamped on to the wire, but the `height` field of the
#' `instrument` object used to represent it has non-zero `height`.
#' The example illustrates this, for mooring in very shallow water.
#'
#' @param instrument a `mooring` object of subtype `instrument`,
#' as created with [instrument()].
#'
#' @return `clamped` returns a copy of its input, but with
#' its `height` value set to 0.
#'
#' @family functions that create mooring objects
#'
#' @examples
#' library(mooring)
#' a <- function(depth) {
#'     anchor("my anchor",
#'         buoyancy = -50, height = 0.1, CD = 0,
#'         depth = depth
#'     )
#' }
#' w <- function(length) wire(length = length)
#' i <- instrument("SBE37 microcat clamp-on style")
#' f <- float("my float", buoyancy = 20, height = 0.2, area = 0.2^2, CD = 1.3)
#' # Construct unclamped mooring (m) and clamped mooring (M).
#' m <- mooring(a(2), w(1), i, w(0.2), f) |>
#'     discretise(by = 0.1) |>
#'     knockdown(u = 1)
#' M <- mooring(a(2), w(1), clamped(i), w(0.2), f) |>
#'     discretise(by = 0.1) |>
#'     knockdown(u = 1)
#' # Plot unclamped case (top row) and clamped case (bottom row).
#' par(mfrow = c(2, 2))
#' plot(m)
#' plot(m, which = "tension")
#' # Bottom row: clamped version
#' plot(M)
#' plot(M, which = "tension")
#'
#' @export
#'
#' @author Dan Kelley
clamped <- function(instrument) {
    if (!inherits(instrument, "mooring") || !inherits(instrument, "instrument")) {
        stop("instrument must be a 'mooring' class object, with subclass 'instrument'")
    }
    instrument$height <- 0
    instrument
} # clamped()
