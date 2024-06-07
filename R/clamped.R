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
#' the `height` value set to 0.
#'
#' @family functions that create mooring objects
#'
#' @examples
#' library(mooring)
#' # For clarity of the example, invent an anchor with no height,
#' # and a float with no height. Then compare the mooring height
#' # without clamping and with clamping.
#' a <- anchor("my anchor", buoyancy = -50, height = 0, CD = 0)
#' w <- function(length) wire(length = length)
#' i <- instrument("SBE37 microcat clamp-on style")
#' f <- float("my float", buoyancy = 20, height = 0, area = 0.2^2, CD = 1.3)
#'
#' # Construct unclamped mooring (m) and clamped mooring (M).
#' m <- mooring(a, w(20), i, w(20), f, waterDepth = 50)
#' M <- mooring(a, w(20), clamped(i), w(20), f, waterDepth = 50)
#'
#' # Compute the mooring lengths by summing the element heights.
#' # Notice that M is 40m long (the sum of the wire lengths)
#' # but that m is longer, because the instrument height is
#' # included.
#' sum(sapply(m@elements, \(e) e@height)) # unclamped instrument
#' sum(sapply(M@elements, \(e) e@height)) # clamped instrument
#' i@height # instrument height
#'
#' @export
#'
#' @author Dan Kelley
clamped <- function(instrument) {
    if (!is.instrument(instrument)) {
        stop("parameter was not created by instrument()")
    }
    instrument@height <- 0
    instrument
} # clamped()
