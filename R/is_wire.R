# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Indicate which mooring elements are wires
#'
#' @template mTemplate
#'
#' @return a logical vector of the same length as `m`, indicating
#' whether each element is of the `wire` class.
#'
#' @examples
#' library(mooring)
#' m <- mooring(anchor(depth = 120), wire(length = 100), float("HMB 20"))
#' isWire(m)
#'
#' @export
#'
#' @author Dan Kelley
isWire <- function(m) {
    sapply(m, \(i) inherits(i, "wire"))
}
