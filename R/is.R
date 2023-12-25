# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Determine which Mooring Elements are Anchors
#'
#' @template mTemplate
#'
#' @return a logical vector of the same length as `m`, indicating
#' whether each element is of the `anchor` sub-class.
#'
#' @examples
#' library(mooring)
#' m <- mooring(anchor(depth = 120), wire(length = 100), float("HMB 20"))
#' is.anchor(m)
#'
#' @export
#'
#' @author Dan Kelley
is.anchor <- function(m) {
    sapply(m, \(i) inherits(i, "anchor"))
}

#' Determine which Mooring Elements are Floats
#'
#' @template mTemplate
#'
#' @return a logical vector of the same length as `m`, indicating
#' whether each element is of the `float` sub-class.
#'
#' @examples
#' library(mooring)
#' m <- mooring(anchor(depth = 120), wire(length = 100), float("HMB 20"))
#' is.float(m)
#'
#' @export
#'
#' @author Dan Kelley
is.float <- function(m) {
    sapply(m, \(mi) inherits(mi, "float"))
}

#' Determine which Mooring Elements are Instruments
#'
#' @template mTemplate
#'
#' @return a logical vector of the same length as `m`, indicating
#' whether each element is of the `instruent` sub-class.
#'
#' @examples
#' library(mooring)
#' m <- mooring(
#'     anchor(depth = 120), wire(length = 100), instrument("RD ADCP"),
#'     wire(length = 100), float("HMB 20")
#' )
#' is.instrument(m)
#'
#' @export
#'
#' @author Dan Kelley
is.instrument <- function(m) {
    sapply(m, \(i) inherits(i, "instrument"))
}

#' Determine Whether an Object is a Mooring
#'
#' An object is a mooring if it is a list that has more than one element, and
#' if each element inherits from the `"mooring"` class.  For example, the
#' output of [mooring()] is a mooring, but the output of [anchor()] is not.
#' This function is mainly designed for use within the package that that, e.g.
#' [knockdown()] will produce an error if its first argument is not a mooring.
#'
#' @param m an object to be tested
#'
#' @export
is.mooring <- function(m = NULL) {
    is.list(m) && length(m) > 1 && all(sapply(m, function(mi) inherits(mi, "mooring")))
}

#' Determine which Mooring Elements are Wires
#'
#' @template mTemplate
#'
#' @return a logical vector of the same length as `m`, indicating
#' whether each element is of the `wire` sub-class.
#'
#' @examples
#' library(mooring)
#' m <- mooring(anchor(depth = 120), wire(length = 100), float("HMB 20"))
#' is.wire(m)
#'
#' @export
#'
#' @author Dan Kelley
is.wire <- function(m) {
    sapply(m, \(i) inherits(i, "wire"))
}
