# vim:spell:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Summarize a mooring
#'
#' @param object a mooring object, created by [mooring()].
#'
#' @param ... ignored.
#'
#' @examples
#' library(mooring)
#' # Simple case
#' m <- mooring(anchor(depth = 100), wire(length = 80), float("HMB 20"))
#' summary(m)
#' # Illustrate how it collects wire subintervals
#' md <- discretise(m)
#' mdk <- knockdown(md, 0.5)
#' summary(mdk)
#'
#' @export
#'
#' @author Dan Kelley
summary.mooring <- function(object, ...) {
    m <- object # use a more useful name
    if (!isMooring(m)) {
        stop("only works for objects created by mooring()")
    }
    n <- length(m)
    lastWasWire <- FALSE
    wireLength <- 0
    iWireStart <- 0
    for (i in seq_len(n)) {
        mi <- m[[i]]
        if (inherits(mi, "wire")) {
            if (!lastWasWire) {
                iWireStart <- i
            }
            lastWasWire <- TRUE
            wireLength <- wireLength + mi$height
        } else {
            if (lastWasWire) {
                # fake an element (and blank out the location)
                W <- m[[iWireStart]]
                W$height <- wireLength
                # cat("iWireStart=", iWireStart, "\n")
                print(W)
            }
            lastWasWire <- FALSE
            wireLength <- 0
            print(mi)
        }
    }
}

